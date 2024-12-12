export interface OperationRoom {
    id: number;
    roomNumber: string;
    capacity: string;
    roomStatus: RoomStatus;

}

export enum RoomStatus {
    Available = 'Available',
    Occupied = 'Occupied',
    UnderMaintenance = 'UnderMaintenance'
}

export interface OperationRoomView {
    roomNumber: string;
    capacity: string;
    roomStatus: RoomStatus;
}