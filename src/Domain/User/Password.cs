using System;
using DDDSample1.Domain.Shared;
using System.Text.RegularExpressions;

namespace DDDSample1.Domain.User
{

    public class Password : IValueObject
    {
        public string password { get; set; } // O accessor set é público        
        public Password(string password)
        {
            this.password = password;
        }

 
    }

}