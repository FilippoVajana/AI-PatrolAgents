using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GridCreator
{
    class Program
    {
        static void Main(string[] args)
        {
            List<string> inputLines = new List<string>();

            if (args.Length != 0)
            {
                if (File.Exists(args[0]))
                {
                    inputLines = ReadFile(args[0]);
                } 
            }
            else
            {
                inputLines = ReadFile("Grid_0.txt");
            }

            List<string> gridRulesList = MakeGrid(inputLines);

            //debug
            System.Console.WriteLine("Original Grid");
            foreach (string s in inputLines)
                System.Console.WriteLine(s);

            System.Console.WriteLine("Grid Rules");
            foreach (string rule in gridRulesList)
                System.Console.WriteLine(rule);
            //stop
            System.Console.ReadLine();
        }

        private static List<string> ReadFile(string path)
        {
            FileStream fileStream = File.Open(path, FileMode.Open);
            StreamReader reader = new StreamReader(fileStream);

            List<string> inputLines = new List<string>();

            while (!reader.EndOfStream)
                inputLines.Add(reader.ReadLine());

            return inputLines;            
        }

        private static List<string> MakeGrid(List<string> inputLines)
        {
            int gridHeight = inputLines.Count, gridWidth = inputLines.ElementAt(0).Length;
            List<string> gridRulesList = new List<string>();

            //inserisco regola id griglia
            AddGridIdRule(gridRulesList);
            //inserisco regola dimensione griglia
            AddGridDimensionRule(gridHeight, gridWidth, gridRulesList);

            //inserisco regola entità
            int l = 0;
            foreach(string line in inputLines)
            {
                char[] _lineArray = line.ToCharArray();
                for(int c=0; c<_lineArray.Length; c++)
                {
                    if (_lineArray[c].Equals('#'))
                        AddObstaclesRule(c, l, gridRulesList);
                }
                l++;
            }

            return gridRulesList;
        }

        private static void AddGridIdRule(List<string> gridRulesList)
        {
            Random randomNum = new Random(DateTime.Now.Second);
            string gridIdRule = $"id_campo({randomNum.Next()}).";
            gridRulesList.Add(gridIdRule);
        }

        private static void AddGridDimensionRule(int height, int width, List<string> gridRulesList)
        {
            string pNE = string.Format("p(0,{0})", height);
            string pSO = string.Format("p({0},0)", width);
            string dimensionRule = $"area_griglia(area({pNE},{pSO})).";
            gridRulesList.Add(dimensionRule);
        }

        private static void AddObstaclesRule(int x, int y, List<string> gridRulesList)
        {            
            string obstacleRule = $"entita_gioco(ostacolo, p({x},{y})).";
            gridRulesList.Add(obstacleRule);
        } 



        private static void WriteFile(string path)
        {

        }
    }
}
