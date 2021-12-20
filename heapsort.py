import numpy
import random
import time

def heapify(arr, n, i):
      largest = i
      l = 2 * i + 1
      r = 2 * i + 2

      if l < n and  arr[i] < arr[l]:
          largest = l
      if r < n and arr[largest] < arr[r]:
          largest = r

      if largest != i:
          arr[i], arr[largest] = arr[largest], arr[i]
          heapify(arr, n, largest)


def heapSort(arr):
    size = len(arr)
      # CONSTRUCCIO DEL MAX HEAP INICIAL
    for i in range(size//2, -1, -1):
        heapify(arr, size, i)
    for i in range(n-1, 0, -1):
          # equivalent al swap en pyhton3
          arr[i], arr[0] = arr[0], arr[i]
          heapify(arr, i, 0)


n = int(input())
arr = numpy.empty(n, dtype=int) #array de n elements
for i in range(n):
    arr[i] = int(input());


INICI = time.time() #Creem una variable amb el temps abans de l'execució
heapSort(arr)
FINAL = time.time() #Aquesta altre variable guarda el temps un cop acaba el heapsort
print("El array ordenat es el seguent:")
for i in arr:
    print(i," ")
print("El temps d'execucio del algorisme es: " ,FINAL - INICI) #Finalment, la diferencia de FINAL - INICI ens retorna el temps d'execució del algorisme, sense tenir en compte inicialitzacions ni altres factors
