#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <stdlib.h> 



struct Data {
    int index;
    float value;
    float values[6];
};

int main() {
    std::ifstream file("solutionValues.txt"); // Open the file
    if (!file.is_open()) {
        std::cerr << "Failed to open the file." << std::endl;
        return 1;
    }

    std::vector<Data> data_list;
    std::string line;
    Data data;
    int no_of_bodies = 0;
    int line_number = 0;
    int k = 0;
    float dt = 0.0f;
    float G = 0.0f;

    std::getline(file, line);
    std::istringstream iss(line);
    iss >> no_of_bodies >> dt >> G;

    float bodiesMainData[no_of_bodies][6];

    for(int iter= 0; iter<no_of_bodies; iter++){
        std::getline(file, line);
        std::istringstream iss(line);
        for(int iter2= 0; iter2<6; iter2++){
            iss>>bodiesMainData[iter][iter2];
        }
    }


    while ((std::getline(file, line))) {

        std::istringstream iss(line);

        if (line_number == 0) {
            // First line with 3 values
            iss >> no_of_bodies >> dt >> G;
        }else if(line_number <= no_of_bodies){

            

        }
        
         else if (line_number >= 1) {
            // Subsequent lines with 6 values each
            for (int i = 0; i < 5; ++i) {
                iss >> data.values[i];
            }
            data_list.push_back(data); // Store the parsed data
        }

        ++line_number;
    }

    file.close();


    // Print the data for verification
    for (const auto& d : data_list) {
        std::cout << "Bodies Count: " << no_of_bodies << ", dt: " << dt << ", G: "<<G<<", Values: ";
        for (float v : d.values) {
            std::cout << v << " ";
        }
        std::cout << std::endl;
    }

    return 0;
}
