# 8 Queens puzzle solutions using genetic algorithm

import random
from scipy.special import comb
from numpy import argmax

def chromosome_fn(minimum, maximum, chromosome_size):
    """
    Gera um cromossomo com genes
    gerados aleatóriamente.

    Argumentos:
    minimum -- inteiro > 0
    maximum -- inteiro >= minimum
    chromosome_size -- inteiro > 0

    Retorno:
    [x, y, z ...] -- lista de inteiros
    representando o cromossomo.
    """
    return [random.randint(minimum, maximum) for _ in range(chromosome_size)]


def create_population(population_size, minimum,
                      maximum, chromosome_size):
    """
    Gera um cromossomo com genes
    gerados aleatóriamente.

    Argumentos:
    population_size -- inteiro >= 1
    minimum -- inteiro > 0
    maximum -- inteiro >= minimum
    chromosome_size -- inteiro > 0

    Retorno:
    [[x, y, z ...] ... [i, j, w...]] -- lista de listas
    de inteiros representando uma população de
    cromossomos.
    """
    return [chromosome_fn(minimum, maximum, chromosome_size)
            for _ in range(population_size)]


def fitness_fn(chromosome):
    """
    Está função gera pares x, y para cara
    gene no cromossomo. Tais pares de genes
    representam a linha e coluna que estão
    posicionadas as rainhas do tabuleiro.
    Após gerar os pares, a função verifica
    quais rainhas estão em xeque umas com
    as outras através das coordenas dos pares
    de cada uma.
    A quantidade máxima de pares em xeque é
    dada pelo coeficient binominal (n, 2).
    O valor retornado pela função é dado
    pela diferença entre este coeficiente e
    a quantidade de rainhas em xeque.
    
    Argumentos:
    chromosome -- lista de inteiros
    representando o cromossomo.

    Retorno:
    fitness -- inteiro representando a
    qualidade do cromossomo.
    """
    checkmates = 0
    pairs = list(zip(chromosome,
                     [i+1 for i in range(len(chromosome))]))

    for i in range(len(pairs)):
        for j in range(i + 1, len(pairs)):
            x, y = pairs[i]
            w, z = pairs[j]
            if x == w or y == z:
                checkmates += 1
            else:
                dx = abs(x - w)
                dy = abs(y - z)
                if dx == dy:
                    checkmates += 1
    fitness = max_atacking_pairs - checkmates
    return fitness

def roulette_select_parents(population, probabilities, qtd_parents=2):
    """
    Seleciona uma quantidade n de pais
    através do método da roleta viciada.
    
    Argumentos:
    population -- lista de listas de inteiros
    representando uma população.
    
    probabilities -- lista de floats representando
    a fatia de cada indivíduo da população
    
    qtd_parents -- inteiro - representa a quantidade
    de pais a serem selecionados.

    Retorno:
    tuple(parents) -- tupla contendo os n pais
    selecionados pela roleta.
    """
    parents = []
    
    for _ in range(qtd_parents):
        draw = random.random()
        for (i, parent) in enumerate(population):
            if draw <= probabilities[i]:
                parents.append(parent)
                break
    return tuple(parents)

def reproduce(x, y):
    """
    Realiza a reprodução através de
    dois pais x e y através da
    seleção aleatória de um ponto
    de corte.

    Argumentos:
    x --  lista de inteiros
    representando um cromossomo.

    y --  lista de inteiros
    representando um cromossomo.
    
    Retorno [x, y, z ... ] -- lista
    de inteiros representando o
    cromossomo gerado.
    """
    chromosome_size = len(x)
    point = random.randint(1, chromosome_size-1)
    return x[: point] + y[point :]


def mutate(chromosome):
    """
    Realiza a mutação de um cromossomo
    através da seleção aleatória de 
    uma posição do cromossomo e a escolha
    aleatória de um novo gene.

    Argumentos:
    chromosome -- lista de inteiros 
    representando o cromossomo.

    Retorno:
    chromosome -- lista de inteiros 
    representando o cromossomo mutado.
    """
    chromosome_size = len(chromosome)
    point = random.randint(0, chromosome_size - 1)
    new_value = random.randint(1, chromosome_size)
    
    while new_value == chromosome[point]:
        new_value = random.randint(1, chromosome_size)
    chromosome[point] = new_value
    return chromosome

def ga_eight_queens(population, fitness_fn, epochs=100):
    '''
    Algoritmo genético que encontra soluções para o puzzle
    das 8 rainhas. Podendo também ser utilizado para
    encontrar soluções para um tabuleiro de n rainhas.
    O algoritmo executa até encontrar a primeira solução
    ou quando um determinado número de épocas é atingido.
    
    Argumentos:
    population -- lista de listas de inteiros representando
    uma população de cromossomos.
    
    fitness-fn -- função utilizada para calcular a qualidade
    do indivíduo de uma população.

    fitness-fn -- inteiro - número de vezes que o algoritmo
    genético vai executar tentando encontrar uma solução 
    para o problema em questão.
    
    retorno:
    (argmax([fitness_fn(e) for e in population]), population) --
    tupla com o índice do indivíduo com maior fitness e 
    a última população criada contendo o tal índividuo.g
    '''
    i = 0
    solutions = []
    
    while i < epochs:    
        new_population = []
        individual_fitness = [fitness_fn(chromosome) for chromosome in population]
        total_fitness = sum(individual_fitness)
        percentage_fitness = [fitness/total_fitness for fitness in individual_fitness]
        probabilities = [sum(percentage_fitness[:i+1]) for i in range(len(percentage_fitness))]

        if max_atacking_pairs in individual_fitness:
            #print('Found at epoch: ', i)
           # index = argmax(individual_fitness)
            #print(population[index], '\n')
            #solutions.append(population[index])
            break
        
        for _ in range(len(population)):
            x, y = roulette_select_parents(population, probabilities)
            child = reproduce(x, y)
            if random.random() <= mutation_probability:
                child = mutate(child)
            new_population.append(child)
        population = new_population
        i += 1
    #return solutions
    return (argmax([fitness_fn(e) for e in population]), population)

board_size = 8
mutation_probability = 0.05
population_size = 1000
chromosome_size = board_size
minimum = 1
maximum = board_size
epochs = 100
max_atacking_pairs = int(comb(chromosome_size, 2))
population = create_population(population_size, minimum, maximum, chromosome_size)

index_best_individual, population = ga_eight_queens(population, fitness_fn, epochs)
solutions = list(
    filter(lambda individual: max_atacking_pairs == fitness_fn(individual),
           population))
print('soluções: ', solutions)


for epochs in [100, 500, 1000, 10000]:
    population = create_population(population_size, minimum, maximum, chromosome_size)
    _, solutions = ga_eight_queens(population, fitness_fn, epochs)
    print('Epochs: ', epochs )
    s = list(
    filter(lambda individual: max_atacking_pairs == fitness_fn(individual),
           solutions))
    print('soluções: ', s)

