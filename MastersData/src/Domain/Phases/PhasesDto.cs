using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationType
{
    public class PhaseDTO
    {
        public string Id {get;set;}
        public int Duration { get; set; }
        public List<RequiredStaffDTO> RequiredStaff { get; set; }

        public PhaseDTO() { }

        public PhaseDTO(string id,int duration, List<RequiredStaffDTO> requiredStaff)
        {
            this.Id = id;
            this.Duration = duration;
            this.RequiredStaff = requiredStaff;
        }
    }

    public class RequiredStaffDTO
    {
        public string num { get; set; }
        public string Specialization { get; set; }

        public RequiredStaffDTO(string num, string specialization)
        {
            this.num = num;
            this.Specialization = specialization;
        }
    }

}
