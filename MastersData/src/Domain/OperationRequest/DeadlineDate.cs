using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRequest
{
    public class DeadLineDate : IValueObject
    {
        public string deadLineDate { get; private set; }

        public DeadLineDate(string deadLineDate)
        {
            validateDeadLineDate(deadLineDate);
            this.deadLineDate = deadLineDate;
        }

        private void validateDeadLineDate(string deadLineDate)
        {
        }
    }
}
