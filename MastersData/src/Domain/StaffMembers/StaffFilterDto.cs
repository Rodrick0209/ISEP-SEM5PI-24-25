using System;
using Microsoft.VisualBasic;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffFilterDto
    {
        
        public string? Name { get; set; }
        public string? LicenseNumber { get; set; }
        public string? PhoneNumber { get; set; }
        public string? Email { get; set; }
        public string? Specialization { get; set; }

    }
}

