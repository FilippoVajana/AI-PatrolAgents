using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Text.RegularExpressions;

namespace GridCreator
{
    class Program
    {
        static void Main(string[] args)
        {
            //leggo input file
            List<string> inputLines = new List<string>();

            if (args.Length != 0)
            {
                if (File.Exists(args[0]))
                {
                    inputLines = readFile(args[0]);
                } 
            }
            else
            {
                inputLines = readFile("Grid_0.txt");
            }
            //creo lista rules
            List<string> gridRulesList = generateGridRules(inputLines);

            //debug
            /*
            System.Console.WriteLine("Original Grid");
            foreach (string s in inputLines)
                System.Console.WriteLine(s);
            Console.WriteLine();

            System.Console.WriteLine("Grid Rules\n\n");
            */
            foreach (string rule in gridRulesList)
                System.Console.WriteLine(rule);
            Console.WriteLine();

            //stop
            Console.WriteLine("Press a Key To Write Result File");
            System.Console.ReadLine();

            //scrivo file output
            writeRulesFile("campo1.pl", gridRulesList);
        }

        private static List<string> readFile(string path)
        {
            FileStream fileStream = File.Open(path, FileMode.Open);
            StreamReader reader = new StreamReader(fileStream);

            List<string> inputLines = new List<string>();

            while (!reader.EndOfStream)
                inputLines.Add(reader.ReadLine());

            return inputLines;            
        }

        private static void writeRulesFile(string path, List<string> rules)
        {
            FileStream outFile = File.Create(Directory.GetCurrentDirectory() + "\\" + path);
            StreamWriter writer = new StreamWriter(outFile);
            writer.AutoFlush = true;

            foreach(string r in rules)
            {
                writer.WriteLine(r);
            }
            //writer.Flush();
            writer.Close();            
        }

        private static List<string> generateGridRules(List<string> inputLines)
        {
            int gridHeight = inputLines.Count, gridWidth = inputLines.ElementAt(0).Length;//da cambiare in max con linq
            List<string> gridRulesList = new List<string>();
            //inserisco header file
            addHeader(gridRulesList);
            //inserisco regola id griglia
            addGridIdRule(gridRulesList);
            //inserisco regola dimensione griglia
            addGridDimensionRule(gridHeight, gridWidth, gridRulesList);

            //inserisco regola entità            
            int l = 0;
            foreach(string line in inputLines)
            {
                char[] _lineArray = line.ToCharArray();
                for(int c=0; c<_lineArray.Length; c++)
                {
                    if (_lineArray[c].Equals('#'))
                        addObstaclesRule(c, l, gridRulesList);
                    else if (_lineArray[c].Equals('O'))
                        addTargetPositionRule(c, l, gridRulesList);
                    else if (_lineArray[c].Equals('G'))
                        addPlayerPositionRule(c, l, gridRulesList);
                }
                l++;
            }
            //regole sentinelle
            /*
            SentinelManager sManager = new SentinelManager(inputLines);
            sManager.ParseGrid();
            */
            return gridRulesList;
        }

        //refactoring - creare classi per ogni simbolo

        private static void addPlayerPositionRule(int x, int y, List<string> gridRulesList)
        {
            string player = $"giocatore(g1,p({x},{y})).";
            gridRulesList.Add(player);
        }

        private static void addTargetPositionRule(int x, int y, List<string> gridRulesList)
        {
            string target = $"obiettivo(o1,p({x},{y})).";
            gridRulesList.Add(target);
        }

        private static void addHeader(List<string> gridRulesList)
        {
            string header = ":- use_module(library(is_a)).\n:- use_module(sentinella).\n:- use_module(campo_gioco_spec).\n\n";
            gridRulesList.Add(header);
        }
        
        private static void addGridIdRule(List<string> gridRulesList)
        {            
            string gridIdRule = $"id_campo(0).";
            gridRulesList.Add(gridIdRule);
        }

        private static void addGridDimensionRule(int height, int width, List<string> gridRulesList)
        {
            string pNE = string.Format("p(0,{0})", height);
            string pSO = string.Format("p({0},0)", width);
            string dimensionRule = $"area_griglia(area({pNE},{pSO})).";
            gridRulesList.Add(dimensionRule);
        }

        private static void addObstaclesRule(int x, int y, List<string> gridRulesList)
        {            
            string obstacleRule = $"entita_gioco(ostacolo, p({x},{y})).";
            gridRulesList.Add(obstacleRule);
        } 
    }
}
