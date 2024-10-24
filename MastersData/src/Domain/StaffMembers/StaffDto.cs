using System;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;



#nullable enable

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffDto
    {
        public StaffId Id { get; set; }
        public string FullName { get; set; }
        public string LicenseNumber { get; set; }
        public String  SpecializationId { get; set; }
        public String AvailabilitySlotsId { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
        public string Category { get; set; }


        public StaffDto(StaffId id, string fullName, string licenseNumber, String specializationId, String availabilitySlotsId, string email, string phoneNumber, string category)
        {
            this.Id = id;
            this.FullName = fullName;
            this.LicenseNumber = licenseNumber;
            this.SpecializationId = specializationId;
            this.AvailabilitySlotsId = availabilitySlotsId;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.Category = category;
        }
    }
}
