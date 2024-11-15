#nullable enable
using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;
using Microsoft.Extensions.Logging.Configuration;

namespace DDDSample1.Domain.StaffMembers
{
    public class Staff : Entity<StaffId>, IAggregateRoot
    {
        public FullName FullName { get; private set; }
        public LicenseNumber LicenseNumber { get; private set; }
        public String SpecializationId { get; private set; }
        public Email Email { get; private set; }
        public PhoneNumber PhoneNumber { get; private set; }
        public Category Category { get; private set; }

        public StaffStatus status { get; private set; }
        public User.User? User { get; private set; }

        private Staff()
        {
        }

        public Staff(StaffId staffId, string fullName, string licenseNumber, String specializationId, string email, string phoneNumber, string category, string status)
        {
            this.Id = staffId;
            this.FullName = new FullName(fullName);
            this.LicenseNumber = new LicenseNumber(licenseNumber);
            this.SpecializationId = specializationId;
            this.Email = new Email(email);
            this.PhoneNumber = new PhoneNumber(phoneNumber);
            this.Category = Enum.Parse<Category>(category);
            this.status= Enum.Parse<StaffStatus>(status);
        }

        public void ChangeFullName(string fullName)
        {
            this.FullName = new FullName(fullName);
        }

        public void ChangeLicenseNumber(string licenseNumber)
        {
            this.LicenseNumber = new LicenseNumber(licenseNumber);
        }


        public void ChangeEmail(string email)
        {
            this.Email = new Email(email);
        }

        public void ChangePhoneNumber(string phoneNumber)
        {
            this.PhoneNumber = new PhoneNumber(phoneNumber);
        }

        public void Deactivate()
        {
            this.status = StaffStatus.Inactive;
        }
        public void Activate()
        {
            this.status = StaffStatus.Active;
        }
    
    }
}