using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{

    public class LoginFailCounter : IValueObject
    {

        public int loginFailCounter { get; private set; }

        public LoginFailCounter(int loginFailCounter)
        {
            this.loginFailCounter = loginFailCounter;
        }
    
        
    }
}