using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class DateOfSlot : IValueObject
    {
        public DateTime dateOfSlot { get; private set; }

        public DateOfSlot(DateTime dateOfSlot)
        {
            validateDateOfSlot(dateOfSlot);
            this.dateOfSlot = dateOfSlot;
        }


        private void validateDateOfSlot(DateTime dateOfSlot)
        {
            if (dateOfSlot > DateTime.Now)
            {
                throw new ArgumentNullException("Invalid slot date");
            }
        }
    }
}