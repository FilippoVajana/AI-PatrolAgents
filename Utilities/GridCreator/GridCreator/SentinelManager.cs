using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Text.RegularExpressions;

namespace GridCreator
{
    class SentinelManager
    {
        private List<string> grid;
        private List<string> sentinels;
        private SortedList<string, List<int[]>> paths; //ronde di tutte le sentinelle

        public SentinelManager(List<string> inputLines)
        {
            grid = inputLines;
            sentinels = new List<string>();
            paths = new SortedList<string, List<int[]>>();
        }

        public void ParseGrid()
        {
            foreach (string line in grid)
            {
                char[] _line = line.ToCharArray();
                int y = grid.IndexOf(line);
                int x = 0;
                for(; x < _line.Length; x++)
                {
                    Match match = Regex.Match(_line[x].ToString(), @"\d");
                    if(match.Success)
                    {
                        String sID = $"S{_line[x]}";
                        AddSentinel(sID, x , y);
                        AddSentryPath(sID, x, y);
                    }
                }
            }
            //debug
            foreach (var item in paths)
            {
                Console.Write(item.Key + "\t");
                foreach (var value in item.Value)
                {
                    Console.Write(value + " - ");
                    Console.WriteLine();
                }
            }
        }

        private void AddSentinel(String sID, int x, int y)
        {
            if(!sentinels.Contains(sID))
            {
                sentinels.Add(sID);                
            }
        }

        private void AddSentryPath(String sID, int x, int y)
        {
            List<int[]> sentinelPath = null;  //ronda sentinella
            paths.TryGetValue(sID, out sentinelPath);
            if(sentinelPath == null)
                sentinelPath = new List<int[]>();

            int[] pos = { x, y };
            sentinelPath.Add(pos);
            paths.Remove(sID);
            paths.Add(sID, sentinelPath);
        }

        private void SortSentryPath()
        { }
    }


    
}
