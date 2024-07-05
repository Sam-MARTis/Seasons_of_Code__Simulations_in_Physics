#include <SFML/Graphics.hpp>
#include <iostream>
#include <cmath>
#define PI 3.14159265359

using namespace std;

void keyPressed()
{
    cout << "Key pressed";
}
float randVal(float a, float b)
{
    return a + ((rand() / (1.0 + RAND_MAX)) * (b - a));
}
class Agent
{
public:
    float x;
    float y;
    float heading;
    sf::CircleShape circleShape;
    sf::VertexArray dart;
    Agent()
    {
        x = 0;
        y = 0;
        heading = 0;
        circleShape.setRadius(10.f);
        circleShape.setPosition(sf::Vector2f(this->x, this->y));
        circleShape.setFillColor(sf::Color::White);
        // sf:: VertexArray dart(sf::TriangleFan, 4);
        dart.setPrimitiveType(sf::TriangleFan);
        dart.resize(4);
    }

    Agent(float _x, float _y, float _heading)
    {
        x = _x;
        y = _y;
        heading = _heading;
        while (heading > 2 * PI)
        {
            heading -= 2 * PI;
        }
        circleShape.setRadius(10.f);
        circleShape.setPosition(sf::Vector2f(this->x, this->y));
        circleShape.setFillColor(sf::Color::White);
        // sf:: VertexArray dart(sf::TriangleFan, 4);
        dart.setPrimitiveType(sf::TriangleFan);
        dart.resize(4);
    }
    void prepareShape()
    {
        dart[0].position = sf::Vector2f(-10 + this->x, 0 + this->y);
        dart[1].position = sf::Vector2f(-20 + this->x, 10 + this->y);
        dart[2].position = sf::Vector2f(20 + this->x, 0 + this->y);
        dart[3].position = sf::Vector2f(-20 + this->x, -10 + this->y);
    }

    void drawShape(sf::RenderWindow &a)
    {
        prepareShape();
        // a.draw(circleShape);
        a.draw(dart);
        x += 0.1 * cos(heading);
        y += 0.1 * sin(heading);
        // dart.setPosition(sf::Vector2f(x, y));
        // circleShape.setPosition(sf::Vector2f(x, y));
    }
};

void createAgents(Agent agents[], int n)
{
    // Agent agents[n];
    for (int i = 0; i < n; i++)
    {
        agents[i] = Agent(randVal(0, 800), randVal(0, 600), randVal(0, 2 * PI));
    }
    // return &agents[0];
}

int main()
{
    srand(time(NULL));
    sf::ContextSettings settings;
    settings.antialiasingLevel = 16;
    sf::RenderWindow window(sf::VideoMode(800, 600), "First window", sf::Style::Default, settings);
    sf::Thread firstThread(&keyPressed);
    Agent agentList[10];
    // long listLen = sizeof(agentList);
    createAgents(agentList, 10);

    window.setVerticalSyncEnabled(true);

    while (window.isOpen())
    {
        window.clear();
        for (int i = 0; i < 10; i += 1)
        {
            agentList[i].drawShape(window);
        }
        // myAgent.drawShape(window);
        window.display();
        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::KeyPressed)
            {
                firstThread.launch();
                // sf::sleep();
            }
            if (event.type == sf::Event::Closed)
            {
                window.close();
            }
        }
    }
    return 0;
}