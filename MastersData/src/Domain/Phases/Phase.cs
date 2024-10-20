using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Domain.OperationType
{
    public class Phase : Entity<PhasesId>
    {
        public int duration { get; private set; }

        public List<RequiredStaff> requiredStaff { get; private set; }

        private Phase() { }

        public Phase(string id,int duration, List<RequiredStaff> requiredStaff)
        {
            this.Id=new PhasesId(id);
            this.duration = duration;
            this.requiredStaff = requiredStaff;
        }
        
    }
}