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
import opt.ga.SingleCrossOver;
import opt.ga.GenericGeneticAlgorithmProblem;
import opt.ga.GeneticAlgorithmProblem;
import opt.ga.MutationFunction;
import opt.ga.StandardGeneticAlgorithm;
import opt.prob.GenericProbabilisticOptimizationProblem;
import opt.prob.MIMIC;
import opt.prob.ProbabilisticOptimizationProblem;
import shared.FixedIterationTrainer;

/**
 * A test using the flip flop evaluation function
 * @author Andrew Guillory gtg008g@mail.gatech.edu
 * @version 1.0
 */
public class FlipFlopTest {
    // filp flop is for simulated annealing
    /** The n value */
    private static final int N = 80;
    
    public static void main(String[] args) {
        int[] ranges = new int[N];
        Arrays.fill(ranges, 2);
        EvaluationFunction ef = new FlipFlopEvaluationFunction();
        Distribution odd = new DiscreteUniformDistribution(ranges);
        NeighborFunction nf = new DiscreteChangeOneNeighbor(ranges);
        MutationFunction mf = new DiscreteChangeOneMutation(ranges);
        CrossoverFunction cf = new SingleCrossOver();
        Distribution df = new DiscreteDependencyTree(.1, ranges); 
        HillClimbingProblem hcp = new GenericHillClimbingProblem(ef, odd, nf);
        GeneticAlgorithmProblem gap = new GenericGeneticAlgorithmProblem(ef, odd, mf, cf);
        ProbabilisticOptimizationProblem pop = new GenericProbabilisticOptimizationProblem(ef, odd, df);
        
        int mimicIter = 2000;
        int rhcIter = 200000;
        int saIter = 200000;
        int gaIter = 2000;

        RandomizedHillClimbing rhc = new RandomizedHillClimbing(hcp);      
        try {
            String line = "";
            String outPath = "ffRHC.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < rhcIter; i++){
                rhc.train();
                pwtr.println(i + ", " + ef.value(rhc.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("Rhc: " + ef.value(rhc.getOptimal()));
        
        SimulatedAnnealing sa = new SimulatedAnnealing(1, .50, hcp);
        try {
            String line = "";
            String outPath = "ffSA.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < saIter; i++){
                sa.train();
                pwtr.println(i + ", " + ef.value(sa.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("SA: " + ef.value(sa.getOptimal()));
        
        //StandardGeneticAlgorithm ga = new StandardGeneticAlgorithm(200, 100, 20, gap);
	StandardGeneticAlgorithm ga = new StandardGeneticAlgorithm(20, 10, 2, gap);
        try {
            String line = "";
            String outPath = "ffGA.out";
            PrintWriter pwtr = new PrintWriter(new BufferedWriter(new FileWriter(outPath, false)));

            for(int i = 0; i < gaIter; i++){
                ga.train();
                pwtr.println(i + ", " + ef.value(ga.getOptimal()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("GA: " + ef.value(ga.getOptimal()));
        
        //MIMIC mimic = new MIMIC(200, 5, pop);
	MIMIC mimic = new MIMIC(20, 3, pop);
        try {
            String line = "";
            String outPath = "ffMimic.out";
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
