#include<SFML/Graphics.hpp>

#include<iostream>

using namespace std;
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


int main(){
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


    srand(time(NULL));
    sf::ContextSettings settings;
    settings.antialiasingLevel = 16;
    sf::RenderWindow window(sf::VideoMode(800, 600), "First window", sf::Style::Default, settings);

    // long listLen = sizeof(agentList);


    
    window.setVerticalSyncEnabled(true);
    
    

        while (window.isOpen())
    {
        window.clear();

        // myAgent.drawShape(window);
        window.display();
        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
            {
                window.close();
            }
        }
    }
    return 0;
}