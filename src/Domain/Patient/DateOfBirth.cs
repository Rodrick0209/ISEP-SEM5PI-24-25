using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patient
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

            string iso8601Date = dateOfBirth.ToString("o");
            if (!DateTime.TryParse(iso8601Date, null, System.Globalization.DateTimeStyles.RoundtripKind, out _))
            {
            throw new ArgumentException("Date of birth is not in ISO 8601 format");
            }
        }
    }
}