#include "TopazLLVM.h"
#include "TopazParser.hpp"
#include <fstream>

// TODO: refactor and clean

#define GET_DEBUG_INFO start_line, start_column, end_line, end_column, cur_file
#define CLASS_MEMBER_SEP "."
#define MIN_END_COLUMN 1

int TOPAZ_DEBUG_MODE = 0;

extern std::stack<std::string> cur_file;

using namespace std;

extern int yyerror(const char *s);

std::string TopazAddSuggestion(std::string type, std::string suggestion) {
    std::string s(type.length()+3, ' ');
    return  "\n" + s + BOLDYELLOW + suggestion + RESET;
}

std::string TypeToStr(Type* t) {
    if (t->isIntegerTy(1)) return "bool";
    if (t->isIntegerTy(8)) return "char";
    if (t->isIntegerTy())  return "int";
    // TODO: add array type
    if (t->isVoidTy())     return "void";
    if (t->isDoubleTy())   return "float";
    if (t->isPointerTy())  return TypeToStr(((PointerType*)t)->getElementType()) + "ptr";
    if (t->isStructTy())   return ((StructType*)t)->getName().str();
    //unknown type?
    return "unknown";
}

void TopazCompileError(int start_line, int start_column,
        int end_line, int end_column, std::string file,
        std::string type, const std::string& message) {
    // Get line
    std::ifstream f(file);
    std::string start_line_str;
    std::string end_line_str;
    std::string buf;
    f.seekg(std::ios::beg);
    // Get the starting line
    int i;
    for(i = 0; i < start_line; ++i) {
        getline(f, start_line_str);
    }
    // Get the end line
    for(i; i < end_line; ++i) {
        getline(f, end_line_str);
    }
    // Begin the end line with formatting only if end column isnt 0
    
    end_line_str += RESET;
    if (end_column >= end_line_str.length() || end_column <= start_column)
        end_line_str += RESET;
    else if (end_column > 1)
        end_line_str.insert(end_column-1, RESET);

    if (start_column >= 0 && start_column < end_line_str.size())
        end_line_str.insert(start_column, BOLDBLUE);
    
    std::string sep = "";
    if (start_line != end_line - 1)
        sep = "...\n";
    // Print the full formatted error message!!!
    printf("\n\n%sIn file %s, line %d, col %d :%s\n", BOLD, file.c_str(), end_line, start_column, RESET);
    printf("%d | %s\n%s%d | %s\n", start_line, start_line_str.c_str(), sep.c_str(), end_line, end_line_str.c_str());
    printf("%s[%s] %s%s\n", BOLDRED, type.c_str(), message.c_str(), RESET);
    f.close();
    exit(1);
}

Value* callCustomOperatorFunctions(CodeGenContext& context, std::string name, Value* obj, std::vector<Value*>& args)
{
    PointerType* ptr1 = (PointerType*)(obj->getType());
    ptr1 = (PointerType*)(ptr1->getElementType());
    if (ptr1 != NULL)
        ptr1 = (PointerType*)(ptr1->getElementType());
    if (ptr1 != NULL && ptr1->isStructTy()) {
        // Here we know its a struct type
        std::string struct_name = ((StructType*)ptr1)->getName().str();
        if (TOPAZ_DEBUG_MODE) cout << "Custom operator on struct " << struct_name << endl;
        // Call the op__bracket method
        Function *function = context.module->getFunction(struct_name + "." + name);
        // Call the function only if it exists... else, move on
        if (function != NULL) {
            CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
            return call;
        }
    }
    return NULL;
}

void addBuiltinConstants(CodeGenContext& context)
{
    GlobalVariable *true_const =
        new GlobalVariable(
            *context.module, IntegerType::getInt1Ty(MyContext),
            true, GlobalValue::PrivateLinkage,
            ConstantInt::get(Type::getInt1Ty(MyContext), 1), ".true_const");
    
    GlobalVariable *false_const =
        new GlobalVariable(
            *context.module, IntegerType::getInt1Ty(MyContext),
            true, GlobalValue::PrivateLinkage,
            ConstantInt::get(Type::getInt1Ty(MyContext), 0), ".false_const");

    GlobalVariable *null_const =
        new GlobalVariable(
            *context.module, PointerType::getUnqual(Type::getVoidTy(MyContext)),
            true, GlobalValue::PrivateLinkage,
            Constant::getNullValue(PointerType::getUnqual(Type::getVoidTy(MyContext))), ".null_const");

    context.locals()["true"] = true_const;
    context.locals()["null"] = null_const;
    context.locals()["false"] = false_const;
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{   
    /* Create the top level interpreter function to call as entry */
    BasicBlock *bblock = BasicBlock::Create(MyContext, "toplevel");
    (*this).pushBlock(bblock);
    addBuiltinConstants(*this);
    root.codeGen(*this); /* emit bytecode for the toplevel block */
    
    
    /* Print the bytecode in a human-readable format 
       to see if our program compiled properly
     */
    Builder.SetInsertPoint((*this).currentBlock());
    Builder.CreateRetVoid();

    (*this).popBlock();
    // Verify module

    printf("%sAll checks pass for `%s`.%s\n", BOLDGREEN, cur_file.top().c_str(), RESET);

    if (TOPAZ_DEBUG_MODE) {
        legacy::PassManager pm;
        pm.add(createPrintModulePass(outs()));
        pm.run(*module);

        //verifyModule(*module, &errs());
        //std::cout << "\n\n";
    }
}

/* Executes the AST by running the main function */
int CodeGenContext::toBinary(std::string filename) {
    auto TargetTriple = LLVMGetDefaultTargetTriple();
    module->setTargetTriple(TargetTriple);

    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        errs() << Error;
        return 1;
    }

    auto CPU = "generic";
    auto Features = "";

    TargetOptions opt;
    auto RM = Optional<Reloc::Model>();
    auto TheTargetMachine =
        Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

    module->setDataLayout(TheTargetMachine->createDataLayout());

    auto Filename = filename + ".o";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    if (EC) {
        errs() << "Could not open file: " << EC.message();
        return 1;
    }

    legacy::PassManager pass;
    auto FileType = CGFT_ObjectFile;

    if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        errs() << "TheTargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*module);
    dest.flush();

    printf("%sCompiled successfully to `%s`. %s\n", BOLDGREEN, Filename.c_str(), RESET);
    return 0;
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
    std::cout << "\n\n\n\nRunning code...\n";
    ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
    ee->finalizeObject();
    vector<GenericValue> noargs;
    GenericValue v = ee->runFunction(mainFunction, noargs);
    return v;
}

/* -- Type Generation -- */
Type* NBasicType::createType(CodeGenContext& context)
{
    Type* t;
    if (context.typeExists(name)) {
        // Custom types go first: you can override
        t = context.types()[name]->createType(context);
    }
    else if (name.compare("this") == 0 && context.hasParent()) {
        // Refer to the class type itself
        t = PointerType::getUnqual(context.structs()[context.getParent()]->value);
    }
    else if (isupper(name[0]) && context.structExists(name)) {
        // Class types
        t = PointerType::getUnqual(context.structs()[name]->value);
    }
    else if (name.compare("string") == 0) {
        t = Type::getInt8PtrTy(MyContext);
    }
    else if (name.compare("int") == 0) {
        t = Type::getInt64Ty(MyContext);
    }
    else if (name.compare("bool") == 0) {
        t = Type::getInt1Ty(MyContext);
    }
    else if (name.compare("char") == 0) {
        t = Type::getInt8Ty(MyContext);
    }
    else if (name.compare("void") == 0) {
        t = Type::getVoidTy(MyContext);
    }
    else if (name.compare("float") == 0) {
        t = Type::getDoubleTy(MyContext);
    }
    else {
        // um why is this not going here
        TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", "Undefined type @" + name);
    }
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating basic type: " << name << endl;
    return t;
}
std::string NBasicType::stringify(CodeGenContext& context)
{
    if (context.typeExists(name)) {
        return context.types()[name]->stringify(context);
    }
    else if (name.compare("this") == 0 && context.hasParent()) {
        return context.getParent();
    }
    return name;
}

Type* NArrayType::createType(CodeGenContext& context)
{
    Type* t = type.createType(context);
    for (int idx : indices)
        t = ArrayType::get(t, idx);
    return t;
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating array type" << endl;
}

Type* NTypeofExpression::createType(CodeGenContext& context)
{
    // Have to do this to refresh
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating type from expr" << endl;
    if (val != NULL)
        return val->codeGen(context)->getType();
    if (type != NULL)
        return type->createType(context);
}

std::string NTypeofExpression::stringify(CodeGenContext& context) {
    if (val != NULL)
        return TypeToStr(val->codeGen(context)->getType());
    if (type != NULL)
        return TypeToStr(type->createType(context));
}

std::string NTemplateType::stringify(CodeGenContext& context) {
    std::string t = name.name;
    TypeList::const_iterator it;
    for (it = param_types.begin(); it != param_types.end(); it++)
         t += (**it).stringify(context);
    return t;
}

Type* NPointerType::createType(CodeGenContext& context)
{
    Type* t = type.createType(context);
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating pointer type" << endl;
    return PointerType::getUnqual(t);
}

Type* NTemplateType::createType(CodeGenContext& context)
{
    if (!context.templateExists(name.name)) {
        TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", "Template @" + name.name + " not found\n");
    }
    auto temp = context.templates()[name.name];

    IDList::const_iterator it;
    int i = 0;

    std::string type_str = name.name;
    // Temporarily define these types
    for (it = temp->arguments.begin(); it != temp->arguments.end(); it++) {
        // Copy types in case overshadowing
        if (TOPAZ_DEBUG_MODE) printf("Defining type %s...\n", (**it).name.c_str());
        if (context.types()[(**it).name] != NULL) {
            if (TOPAZ_DEBUG_MODE) printf("Found conflicts with type %s, so relocating temporarily...\n", (**it).name.c_str());
            context.types()["_temp_" + (**it).name] = context.types()[(**it).name];
        }
        type_str += param_types[i]->stringify(context);
        context.types()[(**it).name] = param_types[i];
        i++;
    }
    if (TOPAZ_DEBUG_MODE) {
        printf("Checking to see if a poly class should be created...\n");
        printf("Checking for %s\n", type_str.c_str());
    }
    // If this is a poly type, create the type
    Type* t;
    if (context.polyClassExists(name.name)) {
        if (TOPAZ_DEBUG_MODE) printf("Class %s is indeed a variant class! body_override=%d\n", name.name.c_str(), context.create_body_override.top());
        // If the new type is already defined, just return it.
        if (context.structExists(type_str))
            t = PointerType::getUnqual(context.structs()[type_str]->value);
        else {
            // Basically perform the same thing as NClassPrototype
            // Create prototype of the new class first
            context.setParent(type_str);
            auto structure = StructType::create(MyContext, type_str);
            auto init = new StructMetadata({}, structure);
            context.structs()[type_str] = init;
            temp->signature.createType(context);
            delete init;
            structure->setBody(context.structs()["__class_prototype"]->value->elements());
            
            // Needs definition! Make sure to define with "new"
            context.structs()[type_str] = new StructMetadata(context.structs()["__class_prototype"]->members, structure);
            delete context.structs()["__class_prototype"];
            context.resetParent();

            // Get the class type
            t = PointerType::getUnqual(context.structs()[type_str]->value);
        }
        // Then, create the actual bodies: change the classname first then generate
        // Create_body is set as true when object is instantiated with "new"
        if (create_body || context.create_body_override.top()) {
            if (TOPAZ_DEBUG_MODE) printf("Populating the body for %s...\n", name.name.c_str());
            context.poly_classes()[name.name]->id = *new NIdentifier(GET_DEBUG_INFO, type_str, 0);
            context.poly_classes()[name.name]->codeGen(context);
            //context.create_body_override.pop() = false;
        }
    }
    else {
        t = temp->signature.createType(context);
    }
    // Then delete/restore all the types
    IDList::const_iterator it2;
    for (it2 = temp->arguments.begin(); it2 != temp->arguments.end(); it2++) {
        if (TOPAZ_DEBUG_MODE) printf("Restoring defined type %s...\n", (**it2).name.c_str());
        if (context.types()["_temp_" + (**it2).name] != NULL) {
            context.types()[(**it2).name] = context.types()["_temp_" + (**it2).name];
        }
    }
    return t;
}

Type* NFunctionType::createType(CodeGenContext& context)
{
    vector<Type*> paramTypes;
    TypeList::const_iterator it;
    for (it = param_types.begin(); it != param_types.end(); it++) {
        paramTypes.push_back((**it).createType(context));
    }
    Type* t = FunctionType::get(return_type.createType(context), makeArrayRef(paramTypes), false);
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating function type " << endl;
    return t;
}

Type* NClassType::createType(CodeGenContext& context)
{
    vector<Type*> memberTypes;
    std::map<std::string, int> memberIndices;
    VariableList::const_iterator it;
    int i = 0;
    for (it = member_types.begin(); it != member_types.end(); it++) {
        memberTypes.push_back((**it).type.createType(context));
        memberIndices[(**it).id.name] = i;
        i++;
    }
    // Also declare all the prototypes
    StatementList::const_iterator it2;
    for (it2 = member_functions.begin(); it2 != member_functions.end(); it2++) {
        (**it2).codeGen(context);
    }
    auto structure = StructType::create(MyContext, makeArrayRef(memberTypes));
    // Temporary to save some data
    context.structs()["__class_prototype"] = new StructMetadata(memberIndices, structure);
    if (TOPAZ_DEBUG_MODE) printf("Created class type\n");
    return structure;
}

/* -- Code Generation -- */

Value* NSizeofExpression::codeGen(CodeGenContext& context)
{
    Type* t;
    if (val != NULL) {
        t = val->codeGen(context)->getType();
    }
    if (type != NULL) {
        t = type->createType(context);
    }
    return ConstantExpr::getSizeOf(t);
}

Value* NStringifyExpression::codeGen(CodeGenContext& context)
{
    if (val != NULL) {
        if (TOPAZ_DEBUG_MODE) printf("Stringifying value!\n");
        Value* value = val->codeGen(context);
        // Store this temporarily
        // So that we can actually use LoadInst as we would need for member calls and dot ops
        AllocaInst *alloc = new AllocaInst(value->getType(), NULL, "stringify_temp", context.currentBlock());
        auto store = new StoreInst(value, alloc, false, context.currentBlock());
        
        // Check if this is an object that has an "on__stringify" magic method
        // LoadInst* load = Builder.CreateLoad(alloc);
        auto load = new LoadInst(alloc->getType(),
                        alloc, "", false, context.currentBlock());

        Value* obj = load->getPointerOperand();
        std::vector<Value*> args;
        args.push_back(load);

        Value* test = callCustomOperatorFunctions(context, "on__stringify", obj, args);
        if (test != NULL)
            return test;

        // Else, actually stringify the expression
        // If the thing is a string, just return the string
        Type* ty = value->getType();
        if (ty->isPointerTy() && ty->getPointerElementType()->isIntegerTy(8)) {
            return value;
        }
        // Or just make a call to sprintf
        int size = 0;
        std::string format;
        if (ty->isIntegerTy(8)) {
            format = "%c";
            size = 24;
        }
        else if (ty->isIntegerTy()) {
            format = "%d";
            size = 24;
        }
        else if (ty->isDoubleTy()) {
            format = "%f";
            size = 48;
        }
        else {
            // Return string of the type
            //TopazCompileError(GET_DEBUG_INFO, "CastError", "Unable to stringify value of type " + TypeToStr(ty));
            return (new NString("<"+TypeToStr(ty)+">"))->codeGen(context);
        }
        // Create a malloc to actually store the string
        Builder.SetInsertPoint(context.currentBlock());
        auto I = CallInst::CreateMalloc(
            context.currentBlock(), Type::getInt64Ty(MyContext),
            Type::getInt8Ty(MyContext), ConstantInt::get(MyContext, APInt(32, size)), nullptr, nullptr, "");
        Value* malloced = Builder.Insert(I);

        // Add to the list of things we malloced
        context.malloced().push_back(malloced);

        Function* format_func = context.module->getFunction("sprintf");
        std::vector<Value*> args2;
        // Create allocation
        args2.push_back(malloced);
        args2.push_back((new NString(format))->codeGen(context));
        args2.push_back(value);
        CallInst *call = CallInst::Create(format_func, makeArrayRef(args2), "", context.currentBlock());
        return malloced;

    }
    if (type != NULL) {
        // Return type string
        return (new NString(type->stringify(context)))->codeGen(context);
    }
}

Value* NPrintExpression::codeGen(CodeGenContext& context)
{
    if (TOPAZ_DEBUG_MODE) printf("Printing value!\n");
    Value* str = (new NStringifyExpression(GET_DEBUG_INFO, val))->codeGen(context);
    // Print the value
    Function* printfunc = context.module->getFunction("printf");
    std::vector<Value*> args;
    args.push_back(str);
    CallInst::Create(printfunc, makeArrayRef(args), "", context.currentBlock());
    // Print new line
    std::vector<Value*> args2;
    args2.push_back((new NString("\n"))->codeGen(context));
    CallInst::Create(printfunc, makeArrayRef(args2), "", context.currentBlock());

    return NULL; // print returns nothing
}

Value* NExitExpression::codeGen(CodeGenContext& context)
{
    if (TOPAZ_DEBUG_MODE) printf("Exiting program\n");
    Builder.CreateRetVoid();
    return NULL; // print returns nothing
}

Value* NIncrementExpression::codeGen(CodeGenContext& context)
{
    if (context.locals().find(var.name) == context.locals().end()) {
        std::cerr << "undeclared variable " << var.name << endl;
        return NULL;
    }
    Value *incremented = Builder.CreateAdd(Builder.CreateLoad(context.locals()[var.name]), Builder.getInt64(1));
    Value *store = new StoreInst(incremented, context.locals()[var.name], false, context.currentBlock());
    return store;
}

Value* NDecrementExpression::codeGen(CodeGenContext& context)
{
    if (context.locals().find(var.name) == context.locals().end()) {
        std::cerr << "undeclared variable " << var.name << endl;
        return NULL;
    }
    Value *incremented = Builder.CreateSub(Builder.CreateLoad(context.locals()[var.name]), Builder.getInt64(1));
    Value *store = new StoreInst(incremented, context.locals()[var.name], false, context.currentBlock());
    return store;
}

Value* NForStatement::codeGen(CodeGenContext& context)
{
    Builder.SetInsertPoint(context.currentBlock());
    // Initialize the counter
    Value *counter;
    if (init != NULL)
        counter = init->codeGen(context);

    // Make the new basic block for the loop header, inserting after current
    // block.
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder.GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(MyContext, "loop", TheFunction);

    // Insert an explicit fall through from the current block to the LoopBB.
    Builder.CreateBr(LoopBB);

    // Start insertion in LoopBB.
    Builder.SetInsertPoint(LoopBB);
    context.pushBlock(LoopBB);

    // Emit the body of the loop.  This, like any other expr, can change the
    // current BB.  Note that we ignore the value computed by the body, but don't
    // allow an error.
    block.codeGen(context);
    
    // Increment counter only if the initializer is present (indicates a for loop)
    if (step == NULL && init != NULL) {
        Value *incremented = Builder.CreateAdd(Builder.CreateLoad(counter), Builder.getInt64(1));
        Value *store = new StoreInst(incremented, counter, false, context.currentBlock());
    }
    else if (init != NULL) {
        step->codeGen(context);
    }
    // Compute the end condition.
    Value *EndCond = cond.codeGen(context);

    // Convert condition to a bool by comparing non-equal to 0.0.
    EndCond = Builder.CreateICmpEQ(EndCond, Builder.getInt1(1), "loopcond");

    // Create the "after loop" block and insert it.
    BasicBlock *LoopEndBB = Builder.GetInsertBlock();
    BasicBlock *AfterBB =
        BasicBlock::Create(MyContext, "afterloop", TheFunction);

    // Insert the conditional branch into the end of LoopEndBB.
    Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

    // Any new code will be inserted in AfterBB.
    context.popBlock();
    Builder.SetInsertPoint(AfterBB);
    context.pushBlock(AfterBB);
    context.increment_extraneous_count();
    return NULL;
}

Value* NIfStatement::codeGen(CodeGenContext& context)
{
    Builder.SetInsertPoint(context.currentBlock());
    Value* CondV = condition.codeGen(context);
    // Cast if the int value is not a bool
    if (!CondV->getType()->isIntegerTy(1))
        CondV = Builder.CreateIntCast(CondV, Type::getInt1Ty(MyContext), false);
    CondV = Builder.CreateICmpNE(CondV, Builder.getInt1(0), "ifcond");

    Function *TheFunction = context.currentBlock()->getParent();

    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *ThenBB = BasicBlock::Create(MyContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(MyContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(MyContext, "ifcont");
    
    Builder.CreateCondBr(CondV, ThenBB, ElseBB);
    // Emit then value.
    Builder.SetInsertPoint(ThenBB);
    context.pushBlock(ThenBB);
    Value *ThenV = if_true.codeGen(context);
    Builder.CreateBr(MergeBB);
    context.popBlock();

    // Emit else block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);
    context.pushBlock(ElseBB);
    if (if_false != NULL)
        Value *ElseV = if_false->codeGen(context);
    Builder.CreateBr(MergeBB);
    context.popBlock();

    // Emit merge block.
    // Why is the stuff after if statement not going to merge block
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    context.pushBlock(MergeBB);
    // Keep track of how many push blocks without pop blocks we are doing
    // So we can pop all of it when we finish declaring the function.
    context.increment_extraneous_count();
    return NULL;
}

Value* NInteger::codeGen(CodeGenContext& context)
{
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating integer: " << value << endl;
    return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating double: " << value << endl;
    return ConstantFP::get(Type::getDoubleTy(MyContext), value);
}

Value* NChar::codeGen(CodeGenContext& context)
{
    // Generate a constant string
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating char: " << value << endl;
    return Builder.getInt8(value);
}

Value* NString::codeGen(CodeGenContext& context)
{
    // Generate a constant string
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating string: " << value << endl;
    Constant *str = ConstantDataArray::getString(MyContext, value);

    // change false to true if i want it to be constant
    GlobalVariable *var =
        new GlobalVariable(
            *context.module, ArrayType::get(IntegerType::get(MyContext, 8), value.length()+1),
            false, GlobalValue::PrivateLinkage, str, ".const_str");
    Constant *zero =
        Constant::getNullValue(IntegerType::getInt32Ty(MyContext));

    std::vector<Constant*> indices;
    indices.push_back(zero);
    indices.push_back(zero);
    Constant *var_ref = ConstantExpr::getGetElementPtr(
    ArrayType::get(IntegerType::get(MyContext, 8), value.length()+1),
        var, indices);
    return var_ref;
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
    // TODO: return pointer to structs
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating identifier reference: " << name << endl;
    if (context.locals().find(name) == context.locals().end()) {
        // Try seeing if its a function instead
        Function *function = context.module->getFunction(name);
        if (function == NULL) {
            TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", "Undefined variable @" + name);
            return NULL;
        }
        // Get the function value.....?
        return function;
    }
    // If it is a pointer, return a LoadInst but without getting the pointer element type.
    if (pointer_level > 0) {
        return new LoadInst(context.locals()[name]->getType(),
                        context.locals()[name], "", false, context.currentBlock());
    }
    return new LoadInst(context.locals()[name]->getType()->getPointerElementType(),
                        context.locals()[name], "", false, context.currentBlock());
}

Value* NFunctionPointerCall::codeGen(CodeGenContext& context)
{
    std::vector<Value*> args;
    ExpressionList::const_iterator it;
    Value* val;
    // First, check if there is a function with the signature

    Function *function = (Function*)lhs.codeGen(context);
    std::string name = function->getName().str();

    std::string type_str = name + "_";
    for (it = arguments.begin(); it != arguments.end(); it++) {
        val = (**it).codeGen(context);
        type_str += TypeToStr(val->getType());
        args.push_back(val);
    }

    if (function == NULL) {
        // Check if it's a poly function by appending signature
        if (TOPAZ_DEBUG_MODE) cout << "Checking if call has variant: " << type_str << endl;
        auto test = context.module->getFunction(type_str);
        if (test != NULL) {
            if (TOPAZ_DEBUG_MODE) printf("Variant found, so calling it...\n");
            function = test;
        }
        else {
            std::string msg = "Function @" + name + " or its variant `" + type_str + "` has not been defined";
            if (context.structExists(name)) {
                msg += TopazAddSuggestion("UndeclaredError", "Maybe try adding a `new` in front of the call?");
            }
            TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", msg);
        }
    }

    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating method call: " << name << endl;
    return call;
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
    
    std::vector<Value*> args;
    ExpressionList::const_iterator it;
    Value* val;
    // First, check if there is a function with the signature
    std::string type_str = id.name + "_";
    for (it = arguments.begin(); it != arguments.end(); it++) {
        val = (**it).codeGen(context);
        type_str += TypeToStr(val->getType());
        args.push_back(val);
    }

    Function *function = context.module->getFunction(id.name);
    if (function == NULL) {
        // Check if it's a poly function by appending signature
        if (TOPAZ_DEBUG_MODE) cout << "Checking if call has variant: " << type_str << endl;
        auto test = context.module->getFunction(type_str);
        if (test != NULL) {
            if (TOPAZ_DEBUG_MODE) printf("Variant found, so calling it...\n");
            function = test;
        }
        else {
            std::string msg = "Function @" + id.name + " or its variant `" + type_str + "` has not been defined";
            if (context.structExists(id.name)) {
                msg += TopazAddSuggestion("UndeclaredError", "Maybe try adding a `new` in front of the call?");
            }
            TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", msg);
        }
    }

    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating method call: " << id.name << endl;
    return call;
}

Value* NBitCast::codeGen(CodeGenContext& context)
{
    Value *val = lhs.codeGen(context);
    Type *rhs_type = val->getType();
    Type *lhs_type = rhs.createType(context);

    Instruction::CastOps castop;
    if (lhs_type->isIntegerTy()) {
        if (rhs_type->isIntegerTy()) {
            castop = Instruction::ZExt;
        }
        if (rhs_type->isDoubleTy()) {
            castop = Instruction::FPToSI;
        }
        if (rhs_type->isPointerTy()) {
            castop = Instruction::PtrToInt;
        }
    }
    // Any value to float conversions
    if (lhs_type->isDoubleTy()) {
        if (rhs_type->isIntegerTy()) {
            castop = Instruction::SIToFP;
        }
        if (rhs_type->isDoubleTy()) {
            castop = Instruction::FPExt;
        }
    }
    // Any value to pointer conversions
    if (lhs_type->isPointerTy()) {
        if (rhs_type->isIntegerTy()) {
            castop = Instruction::IntToPtr;
        }
        if (rhs_type->isPointerTy()) {
            castop = Instruction::BitCast;
        }
    }

    return CastInst::Create(castop, val, lhs_type, "casttmp", context.currentBlock());
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
    // Implement automatic type casting when using operators
    Builder.SetInsertPoint(context.currentBlock());
    Value* left = lhs.codeGen(context);
    Value* right = rhs.codeGen(context);

    // Type is determined by the lefthand side
    Type* lhs_type = left->getType();
    Type* rhs_type = right->getType();
    if (lhs_type != rhs_type) {
        if (TOPAZ_DEBUG_MODE) printf("Casting rhs to lhs type...\n");
        Instruction::CastOps castop;
        // Any value to integer conversions
        if (lhs_type->isIntegerTy()) {
            if (rhs_type->isIntegerTy()) {
                castop = Instruction::ZExt;
            }
            if (rhs_type->isDoubleTy()) {
                castop = Instruction::FPToSI;
            }
            if (rhs_type->isPointerTy()) {
                castop = Instruction::PtrToInt;
            }
        }
        // Any value to float conversions
        if (lhs_type->isDoubleTy()) {
            if (rhs_type->isIntegerTy()) {
                castop = Instruction::SIToFP;
            }
            if (rhs_type->isDoubleTy()) {
                castop = Instruction::FPExt;
            }
        }
        // Any value to pointer conversions
        if (lhs_type->isPointerTy()) {
            if (rhs_type->isIntegerTy()) {
                castop = Instruction::IntToPtr;
            }
            if (rhs_type->isPointerTy()) {
                castop = Instruction::BitCast;
            }
        }

        right = CastInst::Create(castop, right, lhs_type, "casttmp", context.currentBlock());
    }

    if (TOPAZ_DEBUG_MODE) std::cout << "Creating binary operation " << op << endl;
    Instruction::BinaryOps instr;
    switch (op) {
        case TPLUS:     instr = lhs_type->isDoubleTy() ? Instruction::FAdd : Instruction::Add; goto math;
        case TMINUS:    instr = lhs_type->isDoubleTy() ? Instruction::FSub : Instruction::Sub; goto math;
        case TMUL:      instr = lhs_type->isDoubleTy() ? Instruction::FMul : Instruction::Mul; goto math;
        case TDIV:      instr = lhs_type->isDoubleTy() ? Instruction::FDiv : Instruction::SDiv; goto math;
        case TMOD:      instr = lhs_type->isDoubleTy() ? Instruction::FRem : Instruction::SRem; goto math;
        /* comparison */
        case TAND:      instr = Instruction::And; goto math;
        case TOR:       instr = Instruction::Or; goto math;
        case TCEQ:      return Builder.CreateCmp(lhs_type->isDoubleTy() ? CmpInst::FCMP_OEQ : CmpInst::ICMP_EQ, left, right, "cmptmp");
        case TCNE:      return Builder.CreateCmp(lhs_type->isDoubleTy() ? CmpInst::FCMP_ONE : CmpInst::ICMP_NE, left, right, "cmptmp");
        case TCLT:      return Builder.CreateCmp(lhs_type->isDoubleTy() ? CmpInst::FCMP_OLT : CmpInst::ICMP_SLT, left, right, "cmptmp");
        case TCLE:      return Builder.CreateCmp(lhs_type->isDoubleTy() ? CmpInst::FCMP_OLE : CmpInst::ICMP_SLE, left, right, "cmptmp");
        case TCGT:      return Builder.CreateCmp(lhs_type->isDoubleTy() ? CmpInst::FCMP_OGT : CmpInst::ICMP_SGT, left, right, "cmptmp");
        case TCGE:      return Builder.CreateCmp(lhs_type->isDoubleTy() ? CmpInst::FCMP_OGE : CmpInst::ICMP_SGE, left, right, "cmptmp");
    }

    return NULL;
math:
    return BinaryOperator::Create(instr, left, 
        right, "", context.currentBlock());
}

Value* NIsTypeOperator::codeGen(CodeGenContext& context)
{
    Type* lhs = val.codeGen(context)->getType();
    Type* rhs = type.createType(context);
    return Builder.getInt1(lhs == rhs);
    
}

Value* NNewExpression::codeGen(CodeGenContext& context)
{
    // First, create the type
    std::string name = signature.stringify(context);
    if (TOPAZ_DEBUG_MODE) cout << "Creating instantiation for..." << name << endl;
    // This already does the error checking
    // If a class is not 
    context.create_body_override.push(true);
    if (TOPAZ_DEBUG_MODE) cout << "Body override is " << context.create_body_override.top() << endl;
    Type* type = signature.createType(context);
    Builder.SetInsertPoint(context.currentBlock());
    context.create_body_override.pop();
    // Error
    if (!context.structExists(name)) {
        auto typeSize = ConstantExpr::getSizeOf(type);
        auto I = CallInst::CreateMalloc(
          context.currentBlock(), Type::getInt64Ty(MyContext),
          type, typeSize, nullptr, nullptr, "");
        Value* val = Builder.Insert(I);

        // Add to the list of things we malloced
        context.malloced().push_back(val);

        return val;
    }
    
    auto typeSize = ConstantExpr::getSizeOf(context.structs()[name]->value);
    auto I = CallInst::CreateMalloc(
      context.currentBlock(), Type::getInt64Ty(MyContext),
      context.structs()[name]->value, typeSize, nullptr, nullptr, "");
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating new instance of " << name << endl;
    
    Value* val = Builder.Insert(I);

    // Try calling the constructor
    std::string type_str = name + CLASS_MEMBER_SEP + "constructor_";

    if (TOPAZ_DEBUG_MODE) printf("Creating new expression for a class...\n");
    std::vector<Value*> args;
    // First argument is the reference to the instance itself
    args.push_back(val);
    ExpressionList::const_iterator it;
    Value* v;
    // First, check if there is a function with the signature
    for (it = arguments.begin(); it != arguments.end(); it++) {
        v = (**it).codeGen(context);
        type_str += TypeToStr(v->getType());
        args.push_back(v);
    }

    Function *function = context.module->getFunction(name + CLASS_MEMBER_SEP + "constructor");
    if (function == NULL) {
        // Check if it's a poly function by appending signature
        if (TOPAZ_DEBUG_MODE) cout << "Checking if call has variant: " << type_str << endl;
        auto test = context.module->getFunction(type_str);
        if (test != NULL) {
            if (TOPAZ_DEBUG_MODE) printf("Variant found, so calling it...\n");
            function = test;
        }
        else {
            // Means that this is just a struct: just return the malloc'd object
            return val;
        }
    }

    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
    if (TOPAZ_DEBUG_MODE) std::cout << "Calling constructor for " << name << endl;
    return val;
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
    Value *val = rhs.codeGen(context);
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating assignment for " << lhs.name << endl;
    if (context.locals().find(lhs.name) == context.locals().end()) {
        // If not found, declare the variable!
        if (TOPAZ_DEBUG_MODE) std::cout << "Creating automatic variable declaration " << lhs.name << endl;
        AllocaInst *alloc = new AllocaInst(val->getType(), NULL, lhs.name.c_str(), context.currentBlock());
        context.locals()[lhs.name] = alloc;
    }

    return new StoreInst(val, context.locals()[lhs.name], false, context.currentBlock());
}

Value* NBlock::codeGen(CodeGenContext& context)
{
    StatementList::const_iterator it;
    Value *last = NULL;
    for (it = statements.begin(); it != statements.end(); it++) {
        if (TOPAZ_DEBUG_MODE) std::cout << "Generating code for " << typeid(**it).name() << endl;
        last = (**it).codeGen(context);
    }
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating block" << endl;
    return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
    if (TOPAZ_DEBUG_MODE) std::cout << "Generating code for " << typeid(expression).name() << endl;
    return expression.codeGen(context);
}

Value* NPredeclareVariant::codeGen(CodeGenContext& context)
{
    if (TOPAZ_DEBUG_MODE) std::cout << "Predeclaring variant " << temp.stringify(context) << endl;
    temp.createType(context);
    return NULL;
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
    if (TOPAZ_DEBUG_MODE) std::cout << "Generating return code for " << typeid(expression).name() << endl;
    Value *returnValue = expression.codeGen(context);
    //context.setCurrentReturnValue(returnValue);
    ReturnInst::Create(MyContext, returnValue, context.currentBlock());
    return returnValue;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
    //TopazCompileError(GET_DEBUG_INFO, "Declaring variables is not allowed");
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating variable declaration " << id.name << endl;
    AllocaInst *alloc = new AllocaInst(type.createType(context), NULL, id.name.c_str(), context.currentBlock());
    context.locals()[id.name] = alloc;
    if (assignmentExpr != NULL) {
        // TODO: move this down?
        NAssignment assn(id, *assignmentExpr);
        Value* val = assn.codeGen(context);

        // Check if there is an "on__assign" constructor as well
        // on_assign : (this, any) -> void
        std::vector<Value*> args;
        args.push_back(alloc);
        args.push_back(val);
        Value* test = callCustomOperatorFunctions(context, "on__assign", alloc, args);
        if (test != NULL)
            return test;
    }

    return alloc;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    FunctionType* ftype = (FunctionType*)signature.createType(context);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    //TopazCompileError(lineno, start_column, end_column, cur_file, "Test error");
    return function;
}

Value* NTemplateDeclaration::codeGen(CodeGenContext& context)
{
    // Do nothing except store the template.
    context.templates()[id.name] = this;
    if (is_poly) {
        context.poly_classes()[id.name] = new NClassDeclaration(
            GET_DEBUG_INFO,
            *new NIdentifier(GET_DEBUG_INFO, "", 0), *new IDList(), *new NType(), *new NBlock(), *new StatementList()
        );
    }
    return NULL;
}


Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
    // Keep track of extraneous pushes for this function
    context.push_extraneous_count();
    // First, check if this function already exists
    Function *function;
    function = context.module->getFunction(id.name.c_str());
    if (function == NULL) {
        // Check if the function has variants...
        // Side note, a poly function MUST be declared first with "poly"
        // You cant instantly define it
        std::string type_str = id.name + "_" + signature.stringify(context);
        if (TOPAZ_DEBUG_MODE)
            cout << "Checking for variant: " << type_str << endl;
        auto test = context.module->getFunction(type_str);
        if (test != NULL) {
            if (TOPAZ_DEBUG_MODE) printf("Variant found!\n");
            // This means the variant exists! Set that as the function
            function = test;
        }
        else {
            Type *ftype = signature.createType(context);
            // If no signature is specified when no existing functions exist, error.
            if (ftype->isVoidTy()) {
                auto msg = "Function @" + id.name + " has not been declared\n                  and no signature has been specified";
                msg += TopazAddSuggestion("DefinitionError", "If defining a variant, make sure to include signatures");
                TopazCompileError(GET_DEBUG_INFO, "DefinitionError", msg);
            }
            // If not, create a new function!
            if (id.name.compare("main") == 0) {
                context.mainFunction = Function::Create((FunctionType*)ftype, GlobalValue::ExternalLinkage, "main", context.module);
                function = context.mainFunction;
            }
            else function = Function::Create((FunctionType*)ftype, GlobalValue::ExternalLinkage, id.name, context.module);
        }
   }
    // Create function body
    BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);
    context.pushBlock(bblock);

    Function::arg_iterator argsValues = function->arg_begin();

    Value* argumentValue;
    
    int i = 0;
    IDList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        // Declare variables within the functions
        
        AllocaInst *alloc = new AllocaInst(function->getFunctionType()->getParamType(i), NULL, (*it)->name.c_str(), context.currentBlock());
        context.locals()[(*it)->name] = alloc;
        
        argumentValue = &*argsValues++;
        argumentValue->setName((*it)->name.c_str());
        StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->name], false, bblock);
        i++;
    }
    
    block.codeGen(context);

    // Automatic freeing of malloced variables

    Builder.SetInsertPoint(context.currentBlock());
    auto malloced = context.malloced();
    for (int i = 0; i < malloced.size(); i++) {
        if (TOPAZ_DEBUG_MODE) printf("Automatically freeing a variable...\n");
        // Gives seg fault for some reason
        auto I = CallInst::CreateFree(malloced[i], context.currentBlock());
        Builder.Insert(I);
    }

    ReturnInst::Create(MyContext, context.getCurrentReturnValue(), context.currentBlock());

    // Pop all extraneous block pushes generated by if and for statements.
    // This will ensure variables will stay in the correct block.
    if (TOPAZ_DEBUG_MODE) std::cout << "Found " << context.get_extraneous_count() << " extraneous block pushes." << endl;
    for (int i = context.get_extraneous_count(); i > 0; i--)
        context.popBlock();

    context.popBlock();
    // No longer need to keep track of extraneous count!
    context.pop_extraneous_count();

    if (TOPAZ_DEBUG_MODE) std::cout << "Creating function: " << id.name << endl;
    return function;
}

Value* NClassPrototype::codeGen(CodeGenContext& context)
{
    // First, set the context parent
    context.setParent(id.name);
    auto structure = StructType::create(MyContext, id.name);
    // Preinitialize so we can use the type within the declaration
    auto init = new StructMetadata({}, structure);
    context.structs()[id.name] = init;
    // Then evaluate the type and declare all the member functions.
    signature.createType(context);
    // Free the initialized struct
    delete init;
    structure->setBody(context.structs()["__class_prototype"]->value->elements());
    
    // Needs definition!! Use "new" operator to formally define all the functions
    context.structs()[id.name] = new StructMetadata(context.structs()["__class_prototype"]->members, structure);
    // Remove the temp prototype
    context.structs()["__class_prototype"] = NULL;
    // Reset parent name
    context.resetParent();
    return NULL;
}

Value* NFunctionPrototype::codeGen(CodeGenContext& context)
{
    FunctionType *ftype = (FunctionType*) signature.createType(context);
    Function *function;
    std::string name;

    // Prepend class name if it exists in context.
    if (context.hasParent()) {
        // Add the parent type to the beginning of the arg list
        // TODO: optimize the following if possible: .vec() is an expensive operation
        auto params = ftype->params().vec();
        params.insert(params.begin(), PointerType::getUnqual(context.structs()[context.getParent()]->value));
        ftype = FunctionType::get(ftype->getReturnType(), makeArrayRef(params), false);
        name = context.getParent() + CLASS_MEMBER_SEP + id.name;
    }
    else name = id.name;

    // If poly, then append name
    if (is_poly) {
        std::string type_str = signature.stringify(context);
        name += "_" + type_str;
    }

    if (name.compare("main") == 0) {
        context.mainFunction = Function::Create(ftype, GlobalValue::ExternalLinkage, "main", context.module);
        function = context.mainFunction;
    }
    else function = Function::Create(ftype, GlobalValue::ExternalLinkage, name.c_str(), context.module);
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating prototype: " << name << endl;
    return function;
}

Value* NTypeDefinition::codeGen(CodeGenContext& context)
{
    context.types()[id.name] = &signature;
    return NULL;
}

Value* NGlobalDeclaration::codeGen(CodeGenContext& context)
{
    // Figure out how to do this correctlyyyy
    /*
    Type* type = signature.createType(context);
    Constant* con = dyn_cast<Constant*>(val.codeGen(context));
    GlobalVariable *var =
         new GlobalVariable(
            *context.module, type,
            true, GlobalValue::PrivateLinkage,
            con, id.name);
    context.locals()[id.name] = var;
    */
    return NULL;
}

Value* NStructDeclaration::codeGen(CodeGenContext& context)
{
    // TODO: allow recursive definition/access
    vector<Type*> memberTypes;
    std::map<std::string, int> memberIndices;
    VariableList::const_iterator it;
    int i = 0;
    for (it = members.begin(); it != members.end(); it++) {
        memberTypes.push_back((**it).type.createType(context));
        memberIndices[(**it).id.name] = i;
        i++;
    }
    auto structure = StructType::create(MyContext, id.name.c_str());
    structure->setBody(memberTypes);
    // TODO: fix the structs structure
    context.structs()[id.name] = new StructMetadata(memberIndices, structure);
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating struct: " << id.name << endl;
    return NULL;
}

Value* NPointerAssignment::codeGen(CodeGenContext& context)
{
    Builder.SetInsertPoint(context.currentBlock());
    // We know that the lhs will be a LoadInst.
    LoadInst* load = (LoadInst*)(lhs.codeGen(context));
    Value* obj = load->getPointerOperand();
    // @obj is the GEP that we're looking for
    return Builder.CreateStore(rhs.codeGen(context), obj);
}

Value* NArrayExpression::codeGen(CodeGenContext& context)
{
    Type* first = arr[0]->codeGen(context)->getType();
    auto typeSize = ConstantExpr::getSizeOf(first);
    // Multiply size of type by length of array
    auto totalSize = BinaryOperator::Create(Instruction::Mul, typeSize, 
        llvm::ConstantInt::get(Type::getInt64Ty(MyContext), arr.size()+1), "", context.currentBlock());
    
    // Create malloc call and initialize array
    Builder.SetInsertPoint(context.currentBlock());
    auto I = CallInst::CreateMalloc(
        context.currentBlock(), Type::getInt64Ty(MyContext),
        first, totalSize, nullptr, nullptr, "");
    Value* malloced = Builder.Insert(I);
    
    // The last element of array is null
    std::vector<Value *> elementIndex = {
        ConstantInt::get(MyContext, APInt(64, arr.size()))};
    // Get location at index i
    Value *GEP = Builder.CreateGEP(malloced, elementIndex);
    Value *store = new StoreInst(Constant::getNullValue(PointerType::getUnqual(Type::getVoidTy(MyContext))), GEP, false, context.currentBlock());

    for (int i = 0; i < arr.size(); i++) {
        std::vector<Value *> elementIndex = {
            ConstantInt::get(MyContext, APInt(64, i))};
        // Get location at index i
        Value *GEP = Builder.CreateGEP(malloced, elementIndex);
        Value *store = new StoreInst(arr[i]->codeGen(context), GEP, false, context.currentBlock());
    }

    // Add to the list of things we malloced
    // TODO: we need push the NAME of the thing that was malloced, not the actual malloc command
    // to the malloced list
    if (TOPAZ_DEBUG_MODE) printf("Adding to a list of malloced items...\n");
    context.malloced().push_back(malloced);

    return malloced;
}

Value* NDotOperator::codeGen(CodeGenContext& context)
{
    Builder.SetInsertPoint(context.currentBlock());
    // First, load the expr
    LoadInst* lhs = (LoadInst*)(expr.codeGen(context));
    
    // Load the actual value (i.e the pointer to the struct)
    Value* obj = lhs->getPointerOperand();
    // We know this is a pointer type.
    // TODO: error checking here
    PointerType* ptr1 = (PointerType*)(obj->getType());
    PointerType* ptr2 = (PointerType*)(ptr1->getElementType());

    // Continuously dereference?
    while (!ptr2->isStructTy()) {
        if (TOPAZ_DEBUG_MODE) printf("Dereferencing...\n");
        ptr2 = (PointerType*)(ptr2->getElementType());
    }
    StructType* structure = (StructType*)(ptr2);
    if (TOPAZ_DEBUG_MODE) structure->print(llvm::errs(), false);

    std::string struct_name = structure->getName().str();

    // Following line for debugging purposes.
    // structure->print(llvm::errs(), nullptr);
    
    // Then, get index
    // Error checking for members here
    int index = context.structs()[struct_name]->getIndexOfMember(id.name);
    // If not found...
    if (index == -1) {
        TopazCompileError(GET_DEBUG_INFO, "NameError", "@"+id.name + " is not a valid member of " + struct_name);
    }
    if (TOPAZ_DEBUG_MODE) printf("%s[%d] = %s\n", struct_name.c_str(), index, id.name.c_str());

    // Then load the member of the struct
    std::vector<Value *> elementIndex = {
        ConstantInt::get(MyContext, APInt(32, 0)),
        ConstantInt::get(MyContext, APInt(32, index))};
    
    Value *GEP = Builder.CreateGEP(lhs, elementIndex);
    // Must return LoadInst
    // TODO: figure out a way where I wont need to create excess load instructions.
    return Builder.CreateLoad(GEP);
}

Value* NAddressAtOperator::codeGen(CodeGenContext& context)
{
    // Get the operand of the load
    Builder.SetInsertPoint(context.currentBlock());
    LoadInst* lhs = (LoadInst*)(expr.codeGen(context));
    Value* obj = lhs->getPointerOperand();
    return obj;
}

Value* NIndirectionOperator::codeGen(CodeGenContext& context)
{
    // Load the pointer
    Builder.SetInsertPoint(context.currentBlock());
    LoadInst* lhs = (LoadInst*)(expr.codeGen(context));
    return new LoadInst(lhs->getType()->getPointerElementType(),
                    lhs, "", false, context.currentBlock());
}

Value* NBracketOperator::codeGen(CodeGenContext& context)
{
    Builder.SetInsertPoint(context.currentBlock());
    // First, load the expr
    // Make sure it is a LoadInst.
    LoadInst* lhs = (LoadInst*)(expr.codeGen(context));
    // Load the actual value (i.e the pointer to the struct)
    Value* obj = lhs->getPointerOperand();
    Value* idx = index.codeGen(context);

    // check if it's callable 
    std::vector<Value*> args;
    args.push_back(lhs);
    args.push_back(idx);
    Value* test = callCustomOperatorFunctions(context, "on__bracket", obj, args);
    if (test != NULL)
        return test;

    std::vector<Value *> elementIndex;
    if (!obj->getType()->isPointerTy())
        elementIndex.push_back(ConstantInt::get(MyContext, APInt(32, 0)));
    
    elementIndex.push_back(idx);
    // TODO: a weird bug where i cant access static arrays????
    Value *GEP = Builder.CreateGEP(lhs, elementIndex);
    return Builder.CreateLoad(GEP);
}

Value* NClassMethodDeclaration::codeGen(CodeGenContext& context)
{
    std::string parent;
    if (!context.hasParent() && classid == NULL) {
        TopazCompileError(GET_DEBUG_INFO, "DefinitionError", "A class member function declaration without a parent is illegal");
    }
    if (classid != NULL)
        parent = classid->name;
    else
        parent = context.getParent();

    // Check if the parent class actually exissts lol
    if (!context.structExists(parent))
        TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", "Unknown class @" + parent);

    std::string name = parent + CLASS_MEMBER_SEP + id.name;

    if (has_signature) {
        // Check if this is a variant
        std::string type_str = name + "_" + signature.stringify(context);
        Function *function = context.module->getFunction(type_str);
        if (function != NULL) {
            // If variant...
            if (TOPAZ_DEBUG_MODE) cout << "Variant found: " << type_str << endl;
            name = type_str;
        }
        else {
            FunctionType *ftype = (FunctionType*) signature.createType(context);
            // Add parent type to beginning of argument types
            auto params = ftype->params().vec();

            params.insert(params.begin(), PointerType::getUnqual(context.structs()[parent]->value));
            ftype = FunctionType::get(ftype->getReturnType(), makeArrayRef(params), false);
            // Create function prototype first
            Function::Create(ftype, GlobalValue::ExternalLinkage, name.c_str(), context.module);    
        }
    }
    // Prepend the class name to the method name
    auto new_id = new NIdentifier(GET_DEBUG_INFO, name, 0);
    // Create the "this" variable to the params
    IDList arguments_copy(arguments);
    arguments_copy.insert(arguments_copy.begin(), new NIdentifier(GET_DEBUG_INFO, "this", 0));
    return (new NFunctionDeclaration(GET_DEBUG_INFO, *new_id, arguments_copy, *(new NBasicType(GET_DEBUG_INFO, "void")), block))->codeGen(context);
}

Value* NInheritedClassMethodDeclaration::codeGen(CodeGenContext& context)
{
    std::string parent;
    parent = class_sig.stringify(context);
    
    // Check if the parent class actually exissts lol
    if (!context.structExists(parent))
        TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", "Unknown class @" + parent);

    std::string name = parent + CLASS_MEMBER_SEP + id.name;

    if (has_signature) {
        // Check if this is a variant
        std::string type_str = name + "_" + signature.stringify(context);
        Function *function = context.module->getFunction(type_str);
        if (function != NULL) {
            // If variant...
            if (TOPAZ_DEBUG_MODE) cout << "Variant found: " << type_str << endl;
            name = type_str;
        }
        else {
            FunctionType *ftype = (FunctionType*) signature.createType(context);
            // Add parent type to beginning of argument types
            auto params = ftype->params().vec();

            params.insert(params.begin(), PointerType::getUnqual(context.structs()[parent]->value));
            ftype = FunctionType::get(ftype->getReturnType(), makeArrayRef(params), false);
            // Create function prototype first
            Function::Create(ftype, GlobalValue::ExternalLinkage, name.c_str(), context.module);    
        }
    }
    // Prepend the class name to the method name
    auto new_id = new NIdentifier(GET_DEBUG_INFO, name, 0);
    // Create the "this" variable to the params
    IDList arguments_copy(arguments);
    arguments_copy.insert(arguments_copy.begin(), new NIdentifier(GET_DEBUG_INFO, "this", 0));
    return (new NFunctionDeclaration(GET_DEBUG_INFO, *new_id, arguments_copy, *(new NBasicType(GET_DEBUG_INFO, "void")), block))->codeGen(context);
}


Value* NClassDeclaration::codeGen(CodeGenContext& context)
{
    // Check if this is defined as polymorphic...
    if (context.polyClassExists(id.name)) {
        // if so, save it and do nothing.
        context.poly_classes()[id.name] = this;
        // When the class is instantiated, the class will generate...
        // under a different name based on types.
        return NULL;
    }
    
    // Generate code for constructor
    if (constructor) {
        auto new_id = new NIdentifier(GET_DEBUG_INFO, id.name + CLASS_MEMBER_SEP + "constructor", 0);
        // Create the "this" variable
        IDList arguments_copy(arguments);
        arguments_copy.insert(arguments_copy.begin(), new NIdentifier(GET_DEBUG_INFO, "this", 0));
        (new NFunctionDeclaration(GET_DEBUG_INFO, *new_id, arguments_copy, *(new NBasicType(GET_DEBUG_INFO, "void")), constructor_block))->codeGen(context);
    }
    // Generate code for the body
    // Set context parent
    context.setParent(id.name);
    StatementList::const_iterator it;
    for (it = body_stmts.begin(); it != body_stmts.end(); it++) {
        if (TOPAZ_DEBUG_MODE) std::cout << "Generating class body: " << typeid(**it).name() << endl;
        (**it).codeGen(context);
    }
    context.resetParent();
    return NULL;
    
}

Value* NMemberCall::codeGen(CodeGenContext& context)
{
    // TODO: Refactor this portion
    Builder.SetInsertPoint(context.currentBlock());
    // First, load the expr
    LoadInst* lhs = (LoadInst*)(expr.codeGen(context));
    // Load the actual value (i.e the pointer to the struct)
    Value* obj = lhs->getPointerOperand();
    // We know this is a pointer type.
    // TODO: error checking here
    PointerType* ptr1 = (PointerType*)(obj->getType());
    PointerType* ptr2 = (PointerType*)(ptr1->getElementType());
    // Continuously dereference?
    while (!ptr2->isStructTy()) {
        if (TOPAZ_DEBUG_MODE) printf("Dereferencing...\n");
        ptr2 = (PointerType*)(ptr2->getElementType());
    }
    StructType* structure = (StructType*)(ptr2);
    
    if (TOPAZ_DEBUG_MODE) structure->print(llvm::errs(), false);

    // Call the actual member method
    std::string class_name = structure->getName().str();
    NIdentifier* new_name = new NIdentifier(GET_DEBUG_INFO, class_name + CLASS_MEMBER_SEP + id.name, 0);

    // Call the method

    std::vector<Value*> args;
    ExpressionList::const_iterator it;
    Value* val;
    // First, check if there is a function with the signature
    args.push_back(lhs);
    std::string type_str = new_name->name + "_";
    for (it = arguments.begin(); it != arguments.end(); it++) {
        val = (**it).codeGen(context);
        type_str += TypeToStr(val->getType());
        args.push_back(val);
    }

    Function *function = context.module->getFunction(new_name->name);
    if (function == NULL) {
        // Check if it's a poly function by appending signature
        if (TOPAZ_DEBUG_MODE) cout << "Checking if call has variant: " << type_str << endl;
        auto test = context.module->getFunction(type_str);
        if (test != NULL) {
            if (TOPAZ_DEBUG_MODE) printf("Variant found, so calling it...\n");
            function = test;
        }
        else {
            std::string msg = "Function @" + id.name + " or its variant `" + type_str + "` has not been defined";
            if (context.structExists(id.name)) {
                msg += TopazAddSuggestion("UndeclaredError", "Maybe try adding a `new` in front of the call?");
            }
            TopazCompileError(GET_DEBUG_INFO, "UndeclaredError", msg);
        }
    }

    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
    if (TOPAZ_DEBUG_MODE) std::cout << "Creating class member call: " << new_name->name << endl;
    
    return call;
}