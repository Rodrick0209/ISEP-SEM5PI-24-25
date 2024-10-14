using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patient
{
    public class Gender : IValueObject
    {
        public string gender { get; private set; }

        public Gender(string gender)
        {
            validateGender(gender);
            this.gender = gender;
        }

        private void validateGender(String gender)
        {
            List<string> genders = new List<string> { "male", "female" };
            if (string.IsNullOrEmpty(gender) || !genders.Contains(gender))
            {
                throw new ArgumentNullException("Invalid gender");
            }
        }
    }
}