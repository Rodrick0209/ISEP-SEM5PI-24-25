



using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRequest
{
    public class AvailabilitySlotService
    {

        private readonly IUnitOfWork _unitOfWork;

        private readonly IOperationRequestRepository _repo;


        // private readonly IAppointmentRepository _appointmentRepository;

        private readonly IAvailabilitySlotsRepository _availabilitySlotsRepository;
        private readonly IDailyAvailabilityRepository _dailyAvailabilityRepository;


        public AvailabilitySlotService(IUnitOfWork unitOfWork, IAvailabilitySlotsRepository repo, IDailyAvailabilityRepository dailyAvailabilityRepository)
        {
            this._unitOfWork = unitOfWork;
            this._availabilitySlotsRepository = repo;
            this._dailyAvailabilityRepository = dailyAvailabilityRepository;
        }


        public async Task<List<AvailabilitySlot>> GetAllAsync()
        {
            return await this._availabilitySlotsRepository.GetAllAsync();
        }

    }
}