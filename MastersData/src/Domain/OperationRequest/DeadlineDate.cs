using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRequest
{
    public class DeadLineDate : IValueObject
    {
        public string deadLineDate { get; private set; }


        public DeadLineDate(string deadLineDate)
        {
            ValidateDeadLineDate(deadLineDate);
            this.deadLineDate = deadLineDate;
        }

        private void ValidateDeadLineDate(string deadLineDate)
        {
            if (!DateTime.TryParseExact(deadLineDate, "yyyy-MM-dd", null, System.Globalization.DateTimeStyles.None, out DateTime parsedDate))
            {
                throw new ArgumentException("Invalid date format. Expected format is AAAA-MM-DD.");
            }

            if (parsedDate <= DateTime.Now)
            {
                throw new ArgumentException("The deadline date must be in the future.");
            }
        }
    }
}
