using System;
using DDDSample1.Domain.StaffMembers;



namespace DDDSample1.Domain.StaffMembers
{


    public class EditingStaffProfileDto
    {
        public StaffId Id { get; set; }

        public string FullName { get; set; }

        public string LicenseNumber { get; set; }
        public string PhoneNumber { get; set; }
        public string Email { get; set; }




        public EditingStaffProfileDto(StaffId id, string fullname, string licenseNumber, string phoneNumber, string email)
        {
            this.Id = id;
            this.FullName = fullname;
            this.LicenseNumber = licenseNumber;
            this.PhoneNumber = phoneNumber;
            this.Email = email;
        }




    }






}