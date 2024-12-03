using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.RoomTypes
{
    public class SuitableForSurgeries : IValueObject
    {
        public bool suitableForSurgeries { get; private set; }

        public SuitableForSurgeries(bool suitableForSurgeries)
        {
            this.suitableForSurgeries = suitableForSurgeries;
        }
    }
}