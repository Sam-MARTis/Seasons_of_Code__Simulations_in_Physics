#include <SFML/Graphics.hpp>

#include <iostream>

using namespace std;
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <stdlib.h>
#include <math.h>

struct Data
{
    int index;
    float value;
    float values[6];
};
float min(float &a, float &b){
    return a*(a>b) + b*(a<=b);
}

void rotateValue(float &x, float &y, float &z, float angle, float phi){
    float x1 = x;
    float y1 = y;
    float z1 = z;
    x = x1*cos(angle) - y1*sin(angle);
    y = x1*sin(angle) + y1*cos(angle);
    z = z1;
    x1 = x;
    y1 = y;
    z1 = z;
    x = x1*cos(phi) - z1*sin(phi);
    z = x1*sin(phi) + z1*cos(phi);
    y = y1;

}

int main()
{
    const float midPointX = 400;
    const float midPointY = 400;
    const float ScreenMidPointX = 800;
    const float ScreenMidPointY = 500;
    const float cameraPointZ = 1500;




    std::ifstream file("objectStates.txt"); // Open the file
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
    float theta = 0;
    float phi = 0;
    

    std::getline(file, line);
    std::istringstream iss(line);
    iss >> no_of_bodies >> dt >> G;


    float bodiesMainData[no_of_bodies][6];
    sf::CircleShape bodiesData[no_of_bodies];


    for (int iter = 0; iter < no_of_bodies; iter++)
    {
        std::getline(file, line);
        std::istringstream iss(line);
        for (int iter2 = 0; iter2 < 6; iter2++)
        {
            iss >> bodiesMainData[iter][iter2];
        }
    }
        for(int i=0; i< no_of_bodies; i++){
        bodiesData[i] = sf::CircleShape(1.f);
        bodiesData[i].setRadius(bodiesMainData[i][5]);
        bodiesData[i].setFillColor(sf::Color::White);
    }

    srand(time(NULL));
    sf::ContextSettings settings;
    settings.antialiasingLevel = 16;
    sf::RenderWindow window(sf::VideoMode(1600, 1600), "First window", sf::Style::Default, settings);

    // long listLen = sizeof(agentList);

    window.setVerticalSyncEnabled(true);

    while (window.isOpen())
    {
        window.clear();
            theta += 0.001f;
            phi += 0.003f;



        // std::getline(file, line);
        for(int i = 0; i<TIME_SPEEDUP*no_of_bodies; i++){
            std::getline(file, line);
        }
 
        // std::istringstream iss(line);
        float x1, x2, y1, y2, z;
        float blank;
        // iss >> blank >> x1 >> y1 >> x2 >> y2;
        
        for(int i1=0; i1<no_of_bodies; i1++){
            std::getline(file, line);
            std::istringstream iss(line);
            iss>>blank;
            iss >>x1 >> y1;
            z = 0.0;
            x1 = x1 - midPointX;
            y1 = y1 - midPointY;
            rotateValue(x1, y1, z, theta, phi);
            x1 = x1 + ScreenMidPointX;
            y1 = y1 + ScreenMidPointY;
            bodiesData[i1].setPosition(x1, y1);
            bodiesData[i1].setRadius(min(bodiesMainData[i1][5]*(3000000/(float(pow((cameraPointZ-z), 2)))), 10.0f));

            // cout<<i1<<" "<<x1<<" "<<y1<<endl;
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