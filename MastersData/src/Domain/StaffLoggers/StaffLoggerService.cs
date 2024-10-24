using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffLoggers;
using System.Threading.Tasks;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationRequestLoggers
{


    public class StaffLoggerService
    {

        private readonly IUnitOfWork unitOfWork;
        private readonly IStaffLoggerRepository _repo;



        public StaffLoggerService (IUnitOfWork unitOfWork, IStaffLoggerRepository repo)
        {
            this.unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<StaffLogger>> GetAllAsync()
        {    
            return await this._repo.GetAllAsync();
        }




    }









}