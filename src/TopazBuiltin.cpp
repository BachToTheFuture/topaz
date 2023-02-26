#include <iostream>
#include "TopazLLVM.h"

using namespace std;

extern int yyparse();
extern NBlock* programBlock;

using namespace llvm;


Function* createPrintfFunction(CodeGenContext& context)
{
    std::vector<Type*> printf_arg_types;
    printf_arg_types.push_back(Type::getInt8PtrTy(MyContext)); //char*

    FunctionType* printf_type =
        FunctionType::get(
            Type::getInt32Ty(MyContext), printf_arg_types, true);

    Function *func = Function::Create(
                printf_type, Function::ExternalLinkage,
                Twine("printf"),
                context.module
           );
    func->setCallingConv(CallingConv::C);
    return func;
}

Function* createSPrintfFunction(CodeGenContext& context)
{
    std::vector<Type*> printf_arg_types;
    printf_arg_types.push_back(Type::getInt8PtrTy(MyContext)); // dest
    printf_arg_types.push_back(Type::getInt8PtrTy(MyContext)); // formatting

    FunctionType* sprintf_type =
        FunctionType::get(
            Type::getInt32Ty(MyContext), printf_arg_types, true);

    Function *func = Function::Create(
                sprintf_type, Function::ExternalLinkage,
                Twine("sprintf"),
                context.module
           );
    func->setCallingConv(CallingConv::C);
    return func;
}

void createCoreFunctions(CodeGenContext& context){
	Function* printfFn = createPrintfFunction(context);
    Function* sprintfFn = createSPrintfFunction(context);
}
