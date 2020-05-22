using System;
using System.Xml;
using System.IO;
using System.Data.SqlClient;

namespace Zadanie2 {

    public abstract class DataAccessHandler {
        public abstract void Connect();
        public abstract void GetData();
        public abstract void Process();
        public abstract void Close();

        public void Execute() {
            this.Connect();
            this.GetData();
            this.Process();
            this.Close();
        }
    }

    public class SqlDataAccessHandler : DataAccessHandler {
        public string ConnectionString {get;}
        public string Table {get;}
        public string Column {get;}
        SqlConnection connection = null;
        int sum;

        public SqlDataAccessHandler(
            string connectionString, 
            string column, 
            string table
        ) {
            this.ConnectionString = connectionString;
            this.Column = column;
            this.Table  = table;
        }

        public override void Connect() {
            this.connection = new SqlConnection(this.ConnectionString);
        }

        public override void GetData() {
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

        public override void Process() {
            Console.WriteLine(
                "Sum of {0}.{1} is {2}", 
                this.Table, 
                this.Column, 
                this.sum
            );
        }

        public override void Close() {
            this.connection.Close();
        }
    }

    public class XmlDataAccessHandler : DataAccessHandler {
        public string FilePath {get;}
        private XmlDocument doc = null;

        public XmlDataAccessHandler(string fileName) {
            this.FilePath = fileName;
        }

        public override void Connect() {}

        public override void GetData() {
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

        public override void Process() {
            var root = this.doc.DocumentElement;
            var longest = LongestNodeName(root.ChildNodes);
            Console.WriteLine("Longest node name: \"{0}\"", longest);
        }

        public override void Close() {}
    }


    public class Example {
        public static void Main() {
            var xml = new XmlDataAccessHandler("zadanie-2/test.xml");
            xml.Execute();
        }
    }
}