
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;


namespace DDDSample1.Domain.StaffMembers
{

    public interface IStaffService
    {
        Task<StaffDto> AddAsync(CreatingStaffDto staffDto);
        Task<StaffDto> AddAsyncUi(CreatingStaffDto staffDto);
        Task<StaffDto> UpdateAsync(EditingStaffProfileDto dto);
        Task<StaffDto> DeleteAsync(StaffId id);
        Task<List<StaffDto>> GetAllAsync();
        Task<List<StaffDtoUI>> GetAllForUiAsync();
        Task<StaffDto> GetByIdAsync(StaffId id);
        Task<StaffDtoUI> GetByIdForUIAsync(StaffId id);
        Task<List<ViewStaffDto>> SearchAsync(StaffFilterDto staffFilterDto);




    }


}