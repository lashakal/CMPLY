#include <iostream>
#include <string>
using namespace std;

void greet() {
std::cout << "Greetings!" << std::endl;
}
int main() {
for (int x = 5; x <= 10; x++) {
std::cout << "Hello" << std::endl;
}
greet();
int num = 10;
if (num < 12) {
std::cout << "num is less than 12" << std::endl;
}
else {
std::cout << "num is greater than 12" << std::endl;
}
std::cout << num << std::endl;
}
