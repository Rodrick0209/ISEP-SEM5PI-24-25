using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{

    public class AccountBlockedTime : IValueObject
    {

        public DateTime accountBlockedTime { get; private set; }

        public AccountBlockedTime(DateTime accountBlockedTime)
        {
            this.accountBlockedTime = accountBlockedTime;
        }
        
            
        
        
        
        
    }







}