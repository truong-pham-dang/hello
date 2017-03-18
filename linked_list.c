// need g++ compiler, gcc does not work.
#include <stdio.h>

struct Node
{
 int data;
 Node *next;
};

int main()
{
 Node node1;
 Node node2;
 node1.data = 12;
 node2.data = 99;
 node1.next = &node2;
 printf("%d \n", node1.data);
 printf("%d \n", node1.next->data);
 return 0;
}

