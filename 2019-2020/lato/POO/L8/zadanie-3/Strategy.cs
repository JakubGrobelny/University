using System;
using System.Xml;
using System.IO;
using System.Data.SqlClient;

namespace Zadanie3 {

    public interface IDataAccessStrategy {
        void Connect();
        void GetData();
        void Process();
        void Close();
    }

    public class DataAccessHandler {
        private IDataAccessStrategy strategy;

        public DataAccessHandler(IDataAccessStrategy strategy) {
            this.strategy = strategy;
        }

        public void Execute() {
            this.strategy.Connect();
            this.strategy.GetData();
            this.strategy.Process();
            this.strategy.Close();
        }
    }

    public class SqlDataAccessStrategy : IDataAccessStrategy {
        public string ConnectionString {get;}
        public string Table {get;}
        public string Column {get;}
        SqlConnection connection = null;
        int sum;

        public SqlDataAccessStrategy(
            string connectionString, 
            string column, 
            string table
        ) {
            this.ConnectionString = connectionString;
            this.Column = column;
            this.Table  = table;
        }

        public void Connect() {
            this.connection = new SqlConnection(this.ConnectionString);
        }

        public void GetData() {
            var command = new SqlCommand(
                String.Format(
                    "SELECT SUM({0}) FROM {1}", 
                    this.Column, 
                    this.Table
                ),
                this.connection
            );

            this.sum = (int)command.ExecuteScalar();
        }

        public void Process() {
            Console.WriteLine(
                "Sum of {0}.{1} is {2}", 
                this.Table, 
                this.Column, 
                this.sum
            );
        }

        public void Close() {
            this.connection.Close();
        }
    }

    public class XmlDataAccessStrategy : IDataAccessStrategy {
        public string FilePath {get;}
        private XmlDocument doc = null;

        public XmlDataAccessStrategy(string fileName) {
            this.FilePath = fileName;
        }

        public void Connect() {}

        public void GetData() {
            this.doc = new XmlDocument();
            this.doc.Load(this.FilePath);
        }

        private static string LongestNodeName(XmlNodeList nodes) {
            var longest = "";

            foreach (XmlNode node in nodes) {
                if (longest.Length < node.Name.Length) {
                    longest = node.Name;
                }

                var childLongest = LongestNodeName(node.ChildNodes);
                if (childLongest.Length > longest.Length) {
                    longest = childLongest;
                }
            }

            return longest;
        }

        public void Process() {
            var root = this.doc.DocumentElement;
            var longest = LongestNodeName(root.ChildNodes);
            Console.WriteLine("Longest node name: \"{0}\"", longest);
        }

        public void Close() {}
    }

    public class Example {
        public static void Main() {
            var access = new DataAccessHandler(
                new XmlDataAccessStrategy("zadanie-2/test.xml")
            );
            access.Execute();
        }
    }
}