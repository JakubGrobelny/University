using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using _1.Models;

namespace _1.Controllers
{
    public class HomeController : Controller
    {
        public string HelloWorld()
        {
            return "Hello World";
        }

        public int Add(int a, int b)
        {
            return a + b;
        }

        public double Cosine(double x)
        {
            return Math.Cos(x);
        }

        public JsonResult JsonRes()
        {
            return Json(
                new { 
                    first_name = "Jakub", 
                    last_name = "Grobelny" 
                }
            );
        }
    }
}
