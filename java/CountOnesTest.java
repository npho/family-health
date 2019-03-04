package mytest;

import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.BufferedWriter;

import java.util.Arrays;

import dist.DiscreteDependencyTree;
import dist.DiscreteUniformDistribution;
import dist.Distribution;

import opt.DiscreteChangeOneNeighbor;
import opt.EvaluationFunction;
import opt.GenericHillClimbingProblem;
import opt.HillClimbingProblem;
import opt.NeighborFunction;
import opt.RandomizedHillClimbing;
import opt.SimulatedAnnealing;
import opt.example.*;
import opt.ga.CrossoverFunction;
import opt.ga.DiscreteChangeOneMutation;
import opt.ga.GenericGeneticAlgorithmProblem;
import opt.ga.GeneticAlgorithmProblem;
import opt.ga.MutationFunction;
import opt.ga.StandardGeneticAlgorithm;
import opt.ga.UniformCrossOver;
import opt.prob.GenericProbabilisticOptimizationProblem;
import opt.prob.MIMIC;
import opt.prob.ProbabilisticOptimizationProblem;
import shared.FixedIterationTrainer;

/**
 * 
 * @author Andrew Guillory gtg008g@mail.gatech.edu
 * @version 1.0
 */
public class CountOnesTest {
    // count ones for mimic
    /** The n value */
    private static final int N = 80;
    
    public static void main(String[] args) {
        int[] ranges = new int[N];
        Arrays.fill(ranges, 2);
        EvaluationFunction ef = new CountOnesEvaluationFunction();
        Distribution odd = new DiscreteUniformDistribution(ranges);
        NeighborFunction nf = new DiscreteChangeOneNeighbor(ranges);
        MutationFunction mf = new DiscreteChangeOneMutation(ranges);
        CrossoverFunction cf = new UniformCrossOver();
        Distribution df = new DiscreteDependencyTree(.1, ranges); 
        HillClimbingProblem hcp = new GenericHillClimbingProblem(ef, odd, nf);
        GeneticAlgorithmProblem gap = new GenericGeneticAlgorithmProblem(ef, odd, mf, cf);
        ProbabilisticOptimizationProblem pop = new GenericProbabilisticOptimizationProblem(ef, odd, df);
        
        int mimicIter = 10000;
        int rhcIter = 20000;
        int saIter = 20000;
        int gaIter = 30000;

        RandomizedHillClimbing rhc = new RandomizedHillClimbing(hcp);      
        try {
            String line = "";
            String outPath = "countonesRHC.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < rhcIter; i++){
                rhc.train();
                pwtr.println(i + ", " + ef.value(rhc.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("Rhc: " + ef.value(rhc.getOptimal()));
        
        SimulatedAnnealing sa = new SimulatedAnnealing(100, .95, hcp);
        try {
            String line = "";
            String outPath = "countonesSA.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < saIter; i++){
                sa.train();
                pwtr.println(i + ", " + ef.value(sa.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("SA: " + ef.value(sa.getOptimal()));
        
        StandardGeneticAlgorithm ga = new StandardGeneticAlgorithm(20, 20, 0, gap);
        try {
            String line = "";
            String outPath = "countonesGA.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < gaIter; i++){
                ga.train();
                pwtr.println(i + ", " + ef.value(ga.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("GA: " + ef.value(ga.getOptimal()));
        
        MIMIC mimic = new MIMIC(50, 10, pop);
        try {
            String line = "";
            String outPath = "countonesMimic.out";
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
