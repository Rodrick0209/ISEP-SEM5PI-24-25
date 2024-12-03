#nullable enable
using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;

namespace DDDSample1.Domain.RoomTypes
{
    public class RoomType : Entity<RoomTypeId>, IAggregateRoot
    {
        public InternalCode InternalCode { get; private set; }
        public FullName Designation { get; private set; }
        public Description? Description { get; private set; }
        public SuitableForSurgeries SuitableForSurgeries { get; private set; }

        private RoomType()
        {
            this.InternalCode = null!;
            this.Designation = null!;
            this.SuitableForSurgeries = null!;
        }

        public RoomType(string internalCode, string designation, string? description, bool suitableForSurgeries)
        {
            this.Id = new RoomTypeId(Guid.NewGuid());
            this.InternalCode = new InternalCode(internalCode);
            this.Designation = new FullName(designation);
            this.Description = new Description(description);
            this.SuitableForSurgeries = new SuitableForSurgeries(suitableForSurgeries);
        }
    }
}
