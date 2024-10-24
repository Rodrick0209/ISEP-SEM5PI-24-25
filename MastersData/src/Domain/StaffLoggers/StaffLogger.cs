#nullable enable
using System;
using System.Collections.Generic;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffLoggers
{
    public class StaffLogger : Entity<StaffLoggerId>
    {
        public StaffId StaffId { get; private set; }
        public string LicenseNumber { get; private set; }
        public string SpecializationId { get; private set; }
        public string AvailabilitySlotsId { get; private set; }
        public string Email { get; private set; }
        public string PhoneNumber { get; private set; }
        public string Category { get; private set; }
        public DateTime ModificationDate { get; private set; }

        private StaffLogger()
        {

        }

        public StaffLogger(StaffId staffId, string licenseNumber, string specializationId, string availabilitySlotsId, string email, string phoneNumber, string category, DateTime modificationDate)
        {
            this.Id = new StaffLoggerId(Guid.NewGuid());
            this.LicenseNumber = licenseNumber;
            this.SpecializationId = specializationId;
            this.AvailabilitySlotsId = availabilitySlotsId;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.Category = category;
            this.ModificationDate = modificationDate;

        }
    }
}