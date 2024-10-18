using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public class DateOfBirth : IValueObject
    {
        public DateTime dateOfBirth { get; private set; }

        public DateOfBirth(DateTime dateOfBirth)
        {
            validateDateOfBirth(dateOfBirth);
            this.dateOfBirth = dateOfBirth;
        }

        private void validateDateOfBirth(DateTime dateOfBirth)
        {
            if (dateOfBirth > DateTime.Now)
            {
            throw new ArgumentNullException("Invalid date of birth");
            }
        }
    }
}