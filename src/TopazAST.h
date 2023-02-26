#include <iostream>
#include <vector>
#include <stack>
#include <tuple>
#include "TopazColors.h"
#include <llvm/IR/Value.h>

#define DECLARE_DEBUG_INFO_VARS \
	int start_line; \
	int start_column; \
	int end_line; \
	int end_column; \
	std::string cur_file;

#define DEBUG_INFO_PARAMS int start_line, int start_column, \
	int end_line, int end_column, std::string cur_file

#define INIT_DEBUG_INFO cur_file(cur_file), start_line(start_line),\
	end_line(end_line), start_column(start_column), end_column(end_column)

class CodeGenContext;
class NStatement;
class NExpression;
class NType;
class NIdentifier;
class NVariableDeclaration;
class NFunctionPrototype;

typedef std::vector<NStatement*> StatementList;
typedef std::vector<NExpression*> ExpressionList;
typedef std::vector<NType*> TypeList;
typedef std::vector<NVariableDeclaration*> VariableList;
typedef std::vector<NFunctionPrototype*> PrototypeList;
typedef std::vector<NIdentifier*> IDList;

class Node {
public:
	virtual ~Node() {}
	virtual llvm::Value* codeGen(CodeGenContext& context) { return NULL; }
};

class NExpression : public Node {
};

class NType : public Node {
public:
	virtual llvm::Type* createType(CodeGenContext& context) { return NULL; }
	virtual std::string stringify(CodeGenContext& context) { return ""; }
};

class NBasicType : public NType {
public:
	DECLARE_DEBUG_INFO_VARS;
	const std::string& name;
	NBasicType(DEBUG_INFO_PARAMS, const std::string& name) : INIT_DEBUG_INFO, name(name) { }
	virtual llvm::Type* createType(CodeGenContext& context);
	virtual std::string stringify(CodeGenContext& context);
};

class NArrayType : public NType {
public:
	NType& type;
	const std::vector<int>& indices;
	NArrayType(NType& type, const std::vector<int>& indices) :
		type(type), indices(indices) { }
	virtual llvm::Type* createType(CodeGenContext& context);
	virtual std::string stringify(CodeGenContext& context) {
		return type.stringify(context) + "array";
	}
};

class NPointerType : public NType {
public:
	NType& type;
	NPointerType(NType& type) : type(type) { }
	virtual llvm::Type* createType(CodeGenContext& context);
	virtual std::string stringify(CodeGenContext& context) {
		return type.stringify(context) + "ptr";
	}
};

// Defined by the "->" operator.
// lhs -> rhs, where lhs is the param_types, rhs the return type
class NFunctionType : public NType {
public:
	TypeList& param_types;
	NType& return_type;
	NFunctionType(TypeList& param_types, NType& return_type) :
		param_types(param_types), return_type(return_type)  { }
	virtual llvm::Type* createType(CodeGenContext& context);
	virtual std::string stringify(CodeGenContext& context) {
		// Makes it possible to overload functions?
		std::string t = "";
		TypeList::const_iterator it;
		for (it = param_types.begin(); it != param_types.end(); it++)
			 t += (**it).stringify(context);
		// No return types
		return t;
	}
};


class NTypeofExpression : public NType {
public:
	NExpression* val;
	NType* type;
	NTypeofExpression(NExpression* val) : val(val) { type = NULL; }
	NTypeofExpression(NType* type) : type(type) { val = NULL; }
	virtual llvm::Type* createType(CodeGenContext& context);
	virtual std::string stringify(CodeGenContext& context);
};

// Defined by creating a TypeBlock.
// The TypeBlock is automatically sorted into variable and function declarations.
class NClassType : public NType {
public:
	std::string parent_name;
	VariableList& member_types;
	StatementList& member_functions;
	NClassType(VariableList& member_types, StatementList& member_functions) :
		member_types(member_types), member_functions(member_functions) { parent_name = ""; }
	virtual llvm::Type* createType(CodeGenContext& context);
	virtual std::string stringify(CodeGenContext& context) {
		return parent_name;
	}
};

class NTemplateType : public NType {
public:
	DECLARE_DEBUG_INFO_VARS;
	NIdentifier& name;
	TypeList& param_types;
	bool create_body;
	NTemplateType(DEBUG_INFO_PARAMS, NIdentifier& name, TypeList& param_types, bool create_body) :
		INIT_DEBUG_INFO, name(name), param_types(param_types), create_body(create_body) { }
	NTemplateType(DEBUG_INFO_PARAMS, NIdentifier& name, TypeList& param_types) :
		INIT_DEBUG_INFO, name(name), param_types(param_types) { create_body = false; }
	virtual llvm::Type* createType(CodeGenContext& context);
	virtual std::string stringify(CodeGenContext& context);
};

class NStatement : public Node {
};

class NInteger : public NExpression {
public:
	long long value;
	NInteger(long long value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDouble : public NExpression {
public:
	double value;
	NDouble(double value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NString : public NExpression {
public:
	std::string value;
	NString(std::string value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NChar : public NExpression {
public:
	char value;
	NChar(char value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIdentifier : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	std::string name;
	int pointer_level; // 1 indicates pointing, 0 is regular, -1 is dereferencing
	std::vector<int> indices; // empty array indicates nonarray
	NIdentifier(DEBUG_INFO_PARAMS, const std::string& name, int pointer_level, const std::vector<int>& indices = {}) :
		INIT_DEBUG_INFO, name(name), pointer_level(pointer_level), indices(indices) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NMethodCall : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	const NIdentifier& id;
	ExpressionList arguments;
	NMethodCall(DEBUG_INFO_PARAMS, const NIdentifier& id, ExpressionList& arguments) :
		INIT_DEBUG_INFO, id(id), arguments(arguments) { }
	NMethodCall(DEBUG_INFO_PARAMS, const NIdentifier& id) : INIT_DEBUG_INFO, id(id) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionPointerCall : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	NExpression& lhs;
	ExpressionList arguments;
	NFunctionPointerCall(DEBUG_INFO_PARAMS, NExpression& lhs, ExpressionList& arguments) :
		INIT_DEBUG_INFO, lhs(lhs), arguments(arguments) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NNewExpression : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	NType& signature;
	ExpressionList arguments;
	NNewExpression(DEBUG_INFO_PARAMS, NType& signature, ExpressionList& arguments) :
		INIT_DEBUG_INFO, signature(signature), arguments(arguments) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBitCast : public NExpression {
public:
	NExpression& lhs;
	NType& rhs;
	NBitCast(NExpression& lhs, NType& rhs) :
		lhs(lhs), rhs(rhs) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBinaryOperator : public NExpression {
public:
	int op;
	NExpression& lhs;
	NExpression& rhs;
	NBinaryOperator(NExpression& lhs, int op, NExpression& rhs) :
		lhs(lhs), rhs(rhs), op(op) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIsTypeOperator : public NExpression {
public:
	NExpression& val;
	NType& type;
	NIsTypeOperator(NExpression& val, NType& type) :
		val(val), type(type) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NArrayExpression : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	ExpressionList& arr;
	NArrayExpression(DEBUG_INFO_PARAMS, ExpressionList& arr) :
		INIT_DEBUG_INFO, arr(arr) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDotOperator : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	NExpression& expr;
	NIdentifier& id;
	NDotOperator(DEBUG_INFO_PARAMS, NExpression& expr, NIdentifier& id) :
		INIT_DEBUG_INFO, expr(expr), id(id) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBracketOperator : public NExpression {
public:
	NExpression& expr;
	NExpression& index;
	NBracketOperator(NExpression& expr, NExpression& index) :
		expr(expr), index(index) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAddressAtOperator : public NExpression {
public:
	NExpression& expr;
	NAddressAtOperator(NExpression& expr) : expr(expr) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIndirectionOperator : public NExpression {
public:
	NExpression& expr;
	NIndirectionOperator(NExpression& expr) : expr(expr) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NMemberCall : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	NExpression& expr;
	NIdentifier& id;
	ExpressionList arguments;
	NMemberCall(DEBUG_INFO_PARAMS, NExpression& expr, NIdentifier& id, ExpressionList& arguments) :
		INIT_DEBUG_INFO, expr(expr), id(id), arguments(arguments) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignment : public NStatement {
public:
	NIdentifier& lhs;
	NExpression& rhs;
	NAssignment(NIdentifier& lhs, NExpression& rhs) : 
		lhs(lhs), rhs(rhs) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NPointerAssignment : public NStatement {
public:
	NExpression& lhs;
	NExpression& rhs;
	NPointerAssignment(NExpression& lhs, NExpression& rhs) : 
		lhs(lhs), rhs(rhs) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBlock : public NExpression {
public:
	StatementList statements;
	NBlock() { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExpressionStatement : public NStatement {
public:
	NExpression& expression;
	NExpressionStatement(NExpression& expression) : 
		expression(expression) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NPredeclareVariant : public NStatement {
public:
	NTemplateType& temp;
	NPredeclareVariant(NTemplateType& temp) : 
		temp(temp) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIfStatement : public NStatement {
public:
	NExpression& condition;
	NBlock& if_true;
	NBlock *if_false;
	NIfStatement(NExpression& condition, NBlock& if_true) : 
		condition(condition), if_true(if_true) { if_false = NULL; }
	NIfStatement(NExpression& condition, NBlock& if_true, NBlock* if_false) : 
		condition(condition), if_true(if_true), if_false(if_false) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NForStatement : public NStatement {
public:
	NStatement* init;
	NExpression& cond;
	NStatement *step;
	NBlock& block;
	NForStatement(NStatement* init, NExpression& cond, NBlock& block) : 
		init(init), cond(cond), block(block) { step = NULL; }
	NForStatement(NStatement* init, NExpression& cond, NStatement* step, NBlock& block) : 
		init(init), cond(cond), block(block), step(step) { }
	NForStatement( NExpression& cond, NBlock& block) : 
		cond(cond), block(block) { step = NULL; init = NULL; }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NSizeofExpression : public NExpression {
public:
	NExpression* val;
	NType* type;
	NSizeofExpression(NExpression* val) : val(val) { type = NULL; }
	NSizeofExpression(NType* type) : type(type) { val = NULL; }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NStringifyExpression : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	NExpression* val;
	NType* type;
	NStringifyExpression(DEBUG_INFO_PARAMS, NExpression* val) :
		INIT_DEBUG_INFO, val(val) { type = NULL; }
	NStringifyExpression(DEBUG_INFO_PARAMS, NType* type) :
		INIT_DEBUG_INFO, type(type) { val = NULL; }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NPrintExpression : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	NExpression* val;
	NPrintExpression(DEBUG_INFO_PARAMS, NExpression* val) :
		INIT_DEBUG_INFO, val(val) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExitExpression : public NExpression {
public:
	DECLARE_DEBUG_INFO_VARS;
	NExpression* val;
	NExitExpression(DEBUG_INFO_PARAMS, NExpression* val) :
		INIT_DEBUG_INFO, val(val) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIncrementExpression : public NExpression {
public:
	NIdentifier& var;
	NIncrementExpression(NIdentifier& var) : var(var) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDecrementExpression : public NExpression {
public:
	NIdentifier& var;
	NDecrementExpression(NIdentifier& var) : var(var) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NReturnStatement : public NStatement {
public:
	NExpression& expression;
	NReturnStatement(NExpression& expression) : 
		expression(expression) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariableDeclaration : public NStatement {
public:
	NIdentifier& id;
	NType& type;
	NExpression *assignmentExpr;
	NVariableDeclaration(NIdentifier& id, NType& type) :
		type(type), id(id) { assignmentExpr = NULL; }
	NVariableDeclaration(NIdentifier& id, NType& type, NExpression *assignmentExpr) :
		type(type), id(id), assignmentExpr(assignmentExpr) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExternDeclaration : public NStatement {
public:
    const NIdentifier& id;
	NType& signature;

	NExternDeclaration(const NIdentifier& id, NType& signature) :
		id(id), signature(signature) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionDeclaration : public NStatement {
public:
	DECLARE_DEBUG_INFO_VARS;
	const NIdentifier& id;
	IDList arguments;
	NType& signature;
	NBlock& block;
	NFunctionDeclaration(DEBUG_INFO_PARAMS, const NIdentifier& id, 
			const IDList& arguments, NType& signature, NBlock& block) :
		INIT_DEBUG_INFO, id(id), arguments(arguments), signature(signature), block(block) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NTemplateDeclaration : public NStatement {
public:
	DECLARE_DEBUG_INFO_VARS;
	const bool is_poly;
	const NIdentifier& id;
	IDList arguments;
	NType& signature;
	NTemplateDeclaration(DEBUG_INFO_PARAMS, const bool is_poly, const NIdentifier& id, 
			const IDList& arguments, NType& signature) :
		INIT_DEBUG_INFO, is_poly(is_poly), id(id), arguments(arguments), signature(signature) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionPrototype : public NStatement {
public:
	const NIdentifier& id;
	NType& signature;
	bool is_poly;
	NFunctionPrototype(const NIdentifier& id, NType& signature, bool is_poly) :
		id(id), signature(signature), is_poly(is_poly) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NTypeDefinition : public NStatement {
public:
	const NIdentifier& id;
	NType& signature;
	NTypeDefinition(const NIdentifier& id, NType& signature) :
		id(id), signature(signature) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NGlobalDeclaration : public NStatement {
public:
	const NIdentifier& id;
	NType& signature;
	NExpression& val;
	NGlobalDeclaration(const NIdentifier& id, NType& signature, NExpression& val) :
		id(id), signature(signature), val(val) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NClassPrototype : public NStatement {
public:
	const NIdentifier& id;
	NType& signature;
	NClassPrototype(const NIdentifier& id, NType& signature) :
		id(id), signature(signature) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NStructDeclaration : public NStatement {
public:
	const NIdentifier& id;
	VariableList members;
	NStructDeclaration(const NIdentifier& id, const VariableList& members) :
		id(id), members(members) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NClassMethodDeclaration : public NStatement {
public:
	DECLARE_DEBUG_INFO_VARS;
	NIdentifier* classid;
	const NIdentifier& id;
	IDList arguments;
	NType& signature;
	NBlock& block;
	bool has_signature;
	NClassMethodDeclaration(DEBUG_INFO_PARAMS, NIdentifier* classid, const NIdentifier& id, 
			const IDList& arguments, NType& signature, NBlock& block, bool has_signature) :
		INIT_DEBUG_INFO, classid(classid), id(id), arguments(arguments), signature(signature), block(block), has_signature(has_signature) { }
	NClassMethodDeclaration(DEBUG_INFO_PARAMS, const NIdentifier& id, 
			const IDList& arguments, NType& signature, NBlock& block, bool has_signature) :
		INIT_DEBUG_INFO, id(id), arguments(arguments), signature(signature), block(block), has_signature(has_signature) { classid = NULL; }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NInheritedClassMethodDeclaration : public NStatement {
public:
	DECLARE_DEBUG_INFO_VARS;
	NType& class_sig;
	const NIdentifier& id;
	IDList arguments;
	NType& signature;
	NBlock& block;
	bool has_signature;
	NInheritedClassMethodDeclaration(DEBUG_INFO_PARAMS, NType& class_sig, const NIdentifier& id, 
			const IDList& arguments, NType& signature, NBlock& block, bool has_signature) :
		INIT_DEBUG_INFO, class_sig(class_sig), id(id), arguments(arguments), signature(signature), block(block), has_signature(has_signature) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NClassDeclaration : public NStatement {
public:
	DECLARE_DEBUG_INFO_VARS;
	NIdentifier& id;
	IDList arguments;
	NType& signature;
	NBlock& constructor_block;
	StatementList& body_stmts;
	bool constructor = true;
	NClassDeclaration(DEBUG_INFO_PARAMS, NIdentifier& id, 
			const IDList& arguments, NType& signature, NBlock& constructor_block, StatementList& body_stmts) :
		INIT_DEBUG_INFO, id(id), arguments(arguments), signature(signature), constructor_block(constructor_block), body_stmts(body_stmts) { }
	NClassDeclaration(DEBUG_INFO_PARAMS, NIdentifier& id, 
			NType& signature, NBlock& constructor_block, StatementList& body_stmts) :
		INIT_DEBUG_INFO, id(id), signature(signature), constructor_block(constructor_block), body_stmts(body_stmts) {
			arguments = *(new IDList());
			constructor = false;
		}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};


