#include <iostream>
#include "TopazLLVM.h"

using namespace std;

extern int yyparse();
extern FILE *yyin;
extern NBlock* programBlock;
extern int TOPAZ_DEBUG_MODE;

std::stack<std::string> cur_file;

void createCoreFunctions(CodeGenContext& context);

int main(int argc, char **argv)
{
	// Store current file.
	printf("\n%sBeginning code generation for %s.%s\n", BOLDBLUE, std::string(argv[1]).c_str(), RESET);
	cur_file.push(std::string(argv[1]) + ".tpz");
	// Read file
	FILE* current_file = fopen((std::string(argv[1]) + ".tpz").c_str(), "r");
	// Check if debug mode
	if (argc == 3) {
		TOPAZ_DEBUG_MODE = 1;
	}

	yyin = current_file;
	do {
		yyparse();
	} while (!feof(yyin));

    // see http://comments.gmane.org/gmane.comp.compilers.llvm.devel/33877
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();
	CodeGenContext context;
	context.create_body_override.push(false);
	createCoreFunctions(context);
	context.generateCode(*programBlock);
	context.toBinary(argv[1]);
	
	return 0;
}

