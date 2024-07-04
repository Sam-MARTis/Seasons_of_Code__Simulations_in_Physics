#include <SFML/Graphics.hpp>

#include <iostream>

using namespace std;
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <stdlib.h>

struct Data
{
    int index;
    float value;
    float values[6];
};

int main()
{
    std::ifstream file("solutionValues.txt"); // Open the file
    if (!file.is_open())
    {
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
    const int TIME_SPEEDUP = 5;

    std::getline(file, line);
    std::istringstream iss(line);
    iss >> no_of_bodies >> dt >> G;


    float bodiesMainData[no_of_bodies][6];
    sf::CircleShape bodiesData[no_of_bodies];
    for(int i=0; i< no_of_bodies; i++){
        bodiesData[i] = sf::CircleShape(10.f);
        bodiesData[i].setRadius(10.f);
        bodiesData[i].setFillColor(sf::Color::White);
    }

    for (int iter = 0; iter < no_of_bodies; iter++)
    {
        std::getline(file, line);
        std::istringstream iss(line);
        for (int iter2 = 0; iter2 < 6; iter2++)
        {
            iss >> bodiesMainData[iter][iter2];
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


        std::getline(file, line);
        for(int i = 0; i<TIME_SPEEDUP; i++){
            std::getline(file, line);
        }
 
        std::istringstream iss(line);
        float x1, x2, y1, y2;
        float blank;
        // iss >> blank >> x1 >> y1 >> x2 >> y2;
        iss>>blank;
        for(int i1=0; i1<no_of_bodies; i1++){
            iss >>x1 >> y1;
            bodiesData[i1].setPosition(x1, y1);
        }
        // bodiesData[0].setPosition(x1-200, y1-200);
        // bodiesData[1].setPosition(x2-200, y2-200);
        // cout << x1 << " " << y1 << " " << x2 << " " << y2 << endl;
        for(int i2=0; i2<no_of_bodies; i2++){
            window.draw(bodiesData[i2]);
        }
        // window.draw(bodiesData[0]);
        // window.draw(bodiesData[1]);

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