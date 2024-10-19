using System;
using DDDSample1.Domain.Availability;
using DDDSample1.Domain.Specializations;
using Domain.StaffMembers;

#nullable enable

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffDto
    {
        public StaffId Id { get; set; }
        public string FullName { get; set; }
        public string LicenseNumber { get; set; }
        public SpecializationId SpecializationId { get; set; }
        public AvailabilitySlotsId AvailabilitySlotsId { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
        public string Category { get; set; }


        public StaffDto(StaffId id, string fullName, string licenseNumber, SpecializationId specializationId, AvailabilitySlotsId availabilitySlotsId, string email, string phoneNumber, string category)
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
