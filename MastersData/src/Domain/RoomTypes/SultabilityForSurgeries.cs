using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.RoomTypes
{
    public class SultabilityForSurgeries : IValueObject
    {
        public bool sultabilityForSurgeries { get; private set; }

        public SultabilityForSurgeries(bool sultabilityForSurgeries)
        {
            this.sultabilityForSurgeries = sultabilityForSurgeries;
        }
    }
}