#nullable enable
using System;
using System.Collections.Generic;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffLoggers
{
    public class StaffLogger : Entity<StaffLoggerId>,IAggregateRoot
    {
        public string StaffId { get; private set; }
        public string LicenseNumber { get; private set; }
        public string SpecializationId { get; private set; }
        public string Email { get; private set; }
        public string PhoneNumber { get; private set; }
        public string Category { get; private set; }

        public string LoggerType { get; private set; }
        public DateTime ModificationDate { get; private set; }


        private StaffLogger()
        {
        }

        public StaffLogger(string staffId, string licenseNumber, string specializationId, string email, string phoneNumber, string category, string LoggerType, DateTime modificationDate)
        {
            this.Id = new StaffLoggerId(Guid.NewGuid());
            this.StaffId = staffId;
            this.LicenseNumber = licenseNumber;
            this.SpecializationId = specializationId;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.Category = category;
            this.LoggerType = LoggerType;
            this.ModificationDate = modificationDate;
        }
    }
}