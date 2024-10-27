using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationTypes
{
    public class PhaseDTO
    {
        public Guid Id {get;set;}
        public int Duration { get; set; }
        public List<RequiredStaffDTO> RequiredStaff { get; set; }

        public PhaseDTO() { }

        public PhaseDTO(Guid id,int duration, List<RequiredStaffDTO> requiredStaff)
        {
            this.Id = id;
            this.Duration = duration;
            this.RequiredStaff = requiredStaff;
        }

        public PhaseDTO(int duration, List<RequiredStaffDTO> requiredStaff)
        {
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
