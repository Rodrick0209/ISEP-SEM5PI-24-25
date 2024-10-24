#nullable enable
using System;
using System.Collections.Generic;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffLoggers
{
    public class StaffLoggerDto
    {
        public string StaffId { get; private set; }
        public string LicenseNumber { get; private set; }
        public string SpecializationId { get; private set; }
        public string AvailabilitySlotsId { get; private set; }
        public string Email { get; private set; }
        public string PhoneNumber { get; private set; }
        public string Category { get; private set; }
        public string LoggerType { get; private set; }

        public DateTime ModificationDate { get; private set; }


        private StaffLoggerDto()
        {

        }

        public StaffLoggerDto(string staffId, string licenseNumber, string specializationId, string availabilitySlotsId, string email, string phoneNumber, string category, string loggerType, DateTime modificationDate)
        {
            this.StaffId = staffId;
            this.LicenseNumber = licenseNumber;
            this.SpecializationId = specializationId;
            this.AvailabilitySlotsId = availabilitySlotsId;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.Category = category;
            this.LoggerType = loggerType;
            this.ModificationDate = modificationDate;

        }
    }
}