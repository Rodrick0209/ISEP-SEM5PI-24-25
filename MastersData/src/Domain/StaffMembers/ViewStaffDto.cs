using System;
using Microsoft.VisualBasic;

namespace DDDSample1.Domain.StaffMembers
{
    public class ViewStaffDto
    {

        public string Id { get; set; }
        public string FullName { get; set; }
        public string LicenseNumber { get; set; }
        public string PhoneNumber { get; set; }
        public string Email { get; set; }
        public string SpecializationId { get; set; }
        public string Category { get; set; }
        public string Status { get; set; }

    }
}

