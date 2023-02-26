#include <iostream>

#define LINKER_CMD "gcc -o out src/TopazCLib.o "
#define PATH_TO_TPZ2OBJ "./tpz2obj"
#define OBJ_EXTENSION ".o"

/*
Small tool to make life easier compiling + linking Topaz files together
*/

int main(int argc, char **argv)
{
    std::string to_object = "";
    std::string object_file_list = " ";
    // Load up all the Topaz files that we are compiling
	for (int i = 1; i < argc; i++) {
        to_object += std::string(PATH_TO_TPZ2OBJ) + " " + argv[i];
        if (i < argc - 1) to_object += "\n";
        object_file_list += std::string(argv[i]) + OBJ_EXTENSION + " ";
    }
    std::cout << to_object << std::endl;
    std::cout << std::string(LINKER_CMD) + object_file_list << std::endl;
    // Run the commands
    system(to_object.c_str());
    system((std::string(LINKER_CMD) + object_file_list).c_str());
    std::cout << "Compiled to 'out'" << std::endl;
	return 0;
}

