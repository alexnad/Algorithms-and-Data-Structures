#include <iostream>
#include <vector>

void merge_sort(int left, int right, int* array)
{
    if(left == right)
        return;
    int middle = (left + right) / 2;
    merge_sort(left, middle, array);
    merge_sort(middle + 1, right, array);

    int i = left, j = middle+1;
    std::vector<int> temp;

    while(i<=middle || j<=right)
    {
        if(i > middle)
        {
            temp.insert(array[j]);
            j++;
            continue;
        }
        if(j > middle)
        {
            temp.insert(array[i]);
            i++;
            continue;
        }
        if(array[i] <= array[j])
        {
            temp.insert(array[i]); i++;
        }
        else
        {
            temp.insert(array[j]); j++;
        }

    }
    for(i = left; i <= right; i++){
        array[i] = temp[i-left];
    }
    return;
}

int main()
{
    int n;
    std::cin >> n;
    int* array = new int[n];
    for(int i=0; i<n; i++)
    {
        std::cin >> array[i];
    }
    merge_sort(0, n-1, array);
    for(int i=0; i<n; i++)
    {
        std::cout << array[i] << " " << std::endl;
    }
    delete array;

}