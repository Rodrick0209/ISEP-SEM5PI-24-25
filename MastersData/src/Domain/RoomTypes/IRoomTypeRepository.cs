using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.RoomTypes
{
    public interface IRoomTypeRepository : IRepository<RoomType, RoomTypeId>
    {
        Task<RoomType> GetByNameAsync(string name);
    }
}