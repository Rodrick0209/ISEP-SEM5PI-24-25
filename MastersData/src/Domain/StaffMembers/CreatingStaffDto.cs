#nullable enable
using System;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;





namespace DDDSample1.Domain.StaffMembers
{
    public class CreatingStaffDto
    {
        public string FullName { get; set; }
        public string LicenseNumber { get; set; }
        public string SpecializationId { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
        public string Category { get; set; }
       
    }
}
