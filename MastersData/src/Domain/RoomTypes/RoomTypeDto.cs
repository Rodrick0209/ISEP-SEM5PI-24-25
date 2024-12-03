using System;

namespace DDDSample1.Domain.RoomTypes
{
    public class RoomTypeDto
    {
        public Guid Id { get; set; }
        public string InternalCode { get; set; }
        public string Designation { get; set; }
        public string Description { get; set; }
        public bool SultabilityForSurgeries { get; set; }

        public RoomTypeDto(Guid id, string internalCode, string designation, string description, bool sultabilityForSurgeries)
        {
            this.Id = id;
            this.InternalCode = internalCode;
            this.Designation = designation;
            this.Description = description;
            this.SultabilityForSurgeries = sultabilityForSurgeries;
        }
    }
}