package mytest;

import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.util.Arrays;
import java.util.Random;

import dist.DiscreteDependencyTree;
import dist.DiscretePermutationDistribution;
import dist.DiscreteUniformDistribution;
import dist.Distribution;

import opt.SwapNeighbor;
import opt.GenericHillClimbingProblem;
import opt.HillClimbingProblem;
import opt.NeighborFunction;
import opt.RandomizedHillClimbing;
import opt.SimulatedAnnealing;
import opt.example.*;
import opt.ga.CrossoverFunction;
import opt.ga.SwapMutation;
import opt.ga.GenericGeneticAlgorithmProblem;
import opt.ga.GeneticAlgorithmProblem;
import opt.ga.MutationFunction;
import opt.ga.StandardGeneticAlgorithm;
import opt.prob.GenericProbabilisticOptimizationProblem;
import opt.prob.MIMIC;
import opt.prob.ProbabilisticOptimizationProblem;
import shared.FixedIterationTrainer;

/**
 * 
 * @author Andrew Guillory gtg008g@mail.gatech.edu
 * @version 1.0
 */
public class TravelingSalesmanTest {
    // travelling salesman for genetic algorithm
    /** The n value */
    private static final int N = 50;
    /**
     * The test main
     * @param args ignored
     */
    public static void main(String[] args) {
        // travelling salesman for genetic algorithm
        Random random = new Random();
        // create the random points
        double[][] points = new double[N][2];
        for (int i = 0; i < points.length; i++) {
            points[i][0] = random.nextDouble();
            points[i][1] = random.nextDouble();   
        }
        // for rhc, sa, and ga we use a permutation based encoding
        TravelingSalesmanEvaluationFunction ef = new TravelingSalesmanRouteEvaluationFunction(points);
        Distribution odd = new DiscretePermutationDistribution(N);
        NeighborFunction nf = new SwapNeighbor();
        MutationFunction mf = new SwapMutation();
        CrossoverFunction cf = new TravelingSalesmanCrossOver(ef);
        HillClimbingProblem hcp = new GenericHillClimbingProblem(ef, odd, nf);
        GeneticAlgorithmProblem gap = new GenericGeneticAlgorithmProblem(ef, odd, mf, cf);

        int mimicIter = 1000;
        int rhcIter = 200000;
        int saIter = 200000;
        int gaIter = 1000;
        
        RandomizedHillClimbing rhc = new RandomizedHillClimbing(hcp);      
        try {
            String line = "";
            String outPath = "tsRHC.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < rhcIter; i++){
                rhc.train();
                pwtr.println(i + ", " + ef.value(rhc.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("Rhc: " + ef.value(rhc.getOptimal()));
        
        SimulatedAnnealing sa = new SimulatedAnnealing(1E12, .95, hcp);
        try {
            String line = "";
            String outPath = "tsSA.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < saIter; i++){
                sa.train();
                pwtr.println(i + ", " + ef.value(sa.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("SA: " + ef.value(sa.getOptimal()));
        
        StandardGeneticAlgorithm ga = new StandardGeneticAlgorithm(200, 150, 20, gap);
        try {
            String line = "";
            String outPath = "tsGA.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < gaIter; i++){
                ga.train();
                pwtr.println(i + ", " + ef.value(ga.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("GA: " + ef.value(ga.getOptimal()));

        
        // for mimic we use a sort encoding
        ef = new TravelingSalesmanSortEvaluationFunction(points);
        int[] ranges = new int[N];
        Arrays.fill(ranges, N);
        odd = new  DiscreteUniformDistribution(ranges);
        Distribution df = new DiscreteDependencyTree(.1, ranges); 
        ProbabilisticOptimizationProblem pop = new GenericProbabilisticOptimizationProblem(ef, odd, df);
        
        MIMIC mimic = new MIMIC(200, 100, pop);
        // fit = new FixedIterationTrainer(mimic, 1000);
        // fit.train();
        try {
            String line = "";
            String outPath = "tsMimic.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < mimicIter; i++){
                mimic.train();
                pwtr.println(i + ", " + ef.value(mimic.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("Mimic: " + ef.value(mimic.getOptimal()));

    }
}
