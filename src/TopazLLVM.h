#include <stack>
#include <typeinfo>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRPrintingPasses.h>
#include "llvm/Support/FileSystem.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/DebugLoc.h>
#include "llvm/IR/DIBuilder.h"
#include <llvm/IR/DebugInfoMetadata.h>

#include "TopazAST.h"

using namespace llvm;

class NBlock;

static LLVMContext MyContext;
static IRBuilder<> Builder(MyContext);

class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    std::map<std::string, Value*> locals;
};

class StructMetadata {
public:
    StructType* value;
    std::map<std::string, int> members;

    StructMetadata(std::map<std::string, int> members, StructType* value) :
        members(members), value(value) { }

    int getIndexOfMember(std::string name) {
        if(members.find(name) != members.end()) {
            return members[name];
        }
        else {
            return -1;
        } // TODO: create error message when not found
    }
};

class CodeGenContext {
    std::stack<CodeGenBlock *> blocks;
    std::map<std::string, StructMetadata*> _structs;
    std::map<std::string, NType*> _types;
    std::map<std::string, NTemplateDeclaration*> _templates;
    std::map<std::string, NClassDeclaration*> _poly_classes;
    std::map<std::string, Value*> _globals;
    std::stack<int> extraneous_block_pushes;
    std::stack<std::string> parent_class;
    // For automatic freeing
    std::vector<Value*> _malloced;

public:
    Function *mainFunction;
    Module *module;
    std::stack<bool> create_body_override;

    CodeGenContext() { module = new Module("main", MyContext); }
    
    void generateCode(NBlock& root);
    GenericValue runCode();
    int toBinary(std::string filename);
    std::string getParent() { return parent_class.top(); }
    bool hasParent() { return !parent_class.empty(); }
    void setParent(std::string parent) { parent_class.push(parent); }
    void resetParent() { parent_class.pop(); }

    void increment_extraneous_count() {extraneous_block_pushes.top()++;}
    int get_extraneous_count() {return extraneous_block_pushes.top();}
    void push_extraneous_count() {extraneous_block_pushes.push(0);}
    void pop_extraneous_count() {extraneous_block_pushes.pop();}

    std::map<std::string, NTemplateDeclaration*>& templates() { return _templates; }
    bool templateExists(std::string id) { return _templates.find(id) != _templates.end(); }

    std::map<std::string, NType*>& types() { return _types; }
    bool typeExists(std::string id) { return _types.find(id) != _types.end(); }

    std::map<std::string, NClassDeclaration*>& poly_classes() { return _poly_classes; }
    bool polyClassExists(std::string id) { return _poly_classes.find(id) != _poly_classes.end(); }

    std::map<std::string, Value*>& locals() { return blocks.top()->locals; }
    std::vector<Value*>& malloced() { return _malloced; }

    std::map<std::string, StructMetadata*>& structs() { return _structs; }
    bool structExists(std::string id) { return _structs.find(id) != _structs.end(); }

    BasicBlock *currentBlock() { return blocks.top()->block; }
    void pushBlock(BasicBlock *block) {
        // Make sure to also copy variables from the parent block
        // TODO: decide whether to do the same for types
        std::map<std::string, Value*> l;
        bool need_to_copy = false;
        if (!blocks.empty()) {
            l = blocks.top()->locals;
            need_to_copy = true;
        }
        blocks.push(new CodeGenBlock());
        blocks.top()->returnValue = NULL;
        blocks.top()->block = block;
        if (need_to_copy)
            blocks.top()->locals.insert(l.begin(), l.end()); // Copy the local variables as well!!!
    }
    void popBlock() { CodeGenBlock *top = blocks.top(); blocks.pop(); delete top; }
    void setCurrentReturnValue(Value *value) { blocks.top()->returnValue = value; }
    Value* getCurrentReturnValue() { return blocks.top()->returnValue; }
};


