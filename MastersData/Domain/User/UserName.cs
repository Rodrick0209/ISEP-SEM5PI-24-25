using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{

    public class UserName : IValueObject
    {

        public string userName { get; private set; }

        public UserName(string userName)
        {
            if (userName != null)
            {
                userName = userName.Trim();
                validateuserName(userName);
            }
            this.userName = userName;
        }

        private void validateuserName(string userName)
        {
            //Adapt this accordingly with client clarifications about the username
            
            if (string.IsNullOrEmpty(userName) || userName.Length > 20)
            {
                throw new ArgumentNullException(nameof(userName));
            }


        }
    }
}