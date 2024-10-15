using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Utils
{
    public class FullName : IValueObject
    {
        public string fullName { get; private set; }

        public FullName(string fullName)
        {
            validateFullName(fullName);
            fullName.Trim();
            this.fullName = fullName;
        }

        private void validateFullName(string fullName)
        {
            if (string.IsNullOrWhiteSpace(fullName))
            {
                throw new ArgumentNullException("Invalid full name");
            }
        }

        
    }
}