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
            List<String> inputLines = new List<string>();

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

            //debug
            foreach (String s in inputLines)
                System.Console.WriteLine(s);

            //stop
            System.Console.ReadLine();
        }

        private static List<String> ReadFile(string path)
        {
            FileStream fileStream = File.Open(path, FileMode.Open);
            StreamReader reader = new StreamReader(fileStream);

            List<String> inputLines = new List<string>();

            while (!reader.EndOfStream)
                inputLines.Add(reader.ReadLine());

            return inputLines;            
        }

        private static void MakeGrid()
        {

        }

        private static void WriteFile(string path)
        {

        }
    }
}
