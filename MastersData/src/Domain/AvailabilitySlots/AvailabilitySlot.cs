using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.AvailabilitySlots;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlot : Entity<AvailabilitySlotsId>, IAggregateRoot
    {

        public string Name { get; }


        private AvailabilitySlot()
        {
        }



        public AvailabilitySlot(string name)
        {
            this.Id = new AvailabilitySlotsId(Guid.NewGuid());
            Name = name;
        }



    }




}