using System;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;



#nullable enable

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffDtoUI
    {
        public string Id { get; set; }
        public string FullName { get; set; }
        public string LicenseNumber { get; set; }
        public string  SpecializationId { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
        public string Category { get; set; }
        public string status { get; set; }


        public StaffDtoUI(string id, string fullName, string licenseNumber, string specializationId, string email, string phoneNumber, string category, string status)
        {
            this.Id = id;
            this.FullName = fullName;
            this.LicenseNumber = licenseNumber;
            this.SpecializationId = specializationId;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.Category = category;
            this.status = status;
        }
    }
}
