using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using Domain.StaffMembers;

namespace DDDSample1.Domain.StaffMembers
{
    public interface IStaffRepository : IRepository<Staff, StaffId>
    {

    }
}