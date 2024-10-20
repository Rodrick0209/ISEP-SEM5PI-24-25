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
        public SpecializationId SpecializationId { get; private set; }
        public AvailabilitySlotsId AvailabilitySlotsId { get; private set; }
        public Email Email { get; private set; }
        public PhoneNumber PhoneNumber { get; private set; }
        public Category Category { get; private set; }
        public User.User? User { get; private set; }

        private Staff()
        {
        }

        public Staff(StaffId staffId, string fullName, string licenseNumber, SpecializationId specializationId, AvailabilitySlotsId availabilitySlotsId, string email, string phoneNumber, string category)
        {
            this.Id = staffId;
            this.FullName = new FullName(fullName);
            this.LicenseNumber = new LicenseNumber(licenseNumber);
            this.SpecializationId = specializationId;
            this.AvailabilitySlotsId = availabilitySlotsId;
            this.Email = new Email(email);
            this.PhoneNumber = new PhoneNumber(phoneNumber);
            this.Category = Enum.Parse<Category>(category);
        }

        public void ChangeFullName(FullName fullName)
        {
            this.FullName = fullName;
        }

        public void ChangeLicenseNumber(LicenseNumber licenseNumber)
        {
            this.LicenseNumber = licenseNumber;
        }

        public void ChangeSpecialization(SpecializationId specializationId)
        {
            this.SpecializationId = specializationId;
        }

        public void ChangeAvailabilitySlots(AvailabilitySlotsId availabilitySlots)
        {
            this.AvailabilitySlotsId = availabilitySlots;
        }

        public void ChangeEmail(Email email)
        {
            this.Email = email;
        }

        public void ChangePhoneNumber(PhoneNumber phoneNumber)
        {
            this.PhoneNumber = phoneNumber;
        }
        public void ChangeCategory(Category category)
        {
            this.Category = category;
        }
    
    }
}