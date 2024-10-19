using System;
using DDDSample1.Domain.Shared;
using System.Text.RegularExpressions;

namespace DDDSample1.Domain.User
{

    public class Password : IValueObject
    {
        public string password { get; set; }         
        public Password(string password)
        {
            //implementar estas regras -> at least 10 characters long, at least a digit, a capital letter and a special character

            this.password = password;
        }

 
    }

}