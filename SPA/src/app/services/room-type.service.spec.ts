import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { RoomTypeService } from './room-type.service';
import { AddRoomType } from '../models/room-type';

describe('RoomTypeService', () => {
  let service: RoomTypeService;
  let httpMock: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [RoomTypeService]
    });
    service = TestBed.inject(RoomTypeService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should add a room type', () => {
    const mockRoomType: AddRoomType = {
      InternalCode: 'RT001',
      Designation: 'Deluxe Room',
      Description: 'A deluxe room with all amenities',
      SuitableForSurgeries: true
    };

    service.add(mockRoomType).subscribe(response => {
      expect(response).toBeTruthy();
    });

    const req = httpMock.expectOne('/api/roomTypes');
    expect(req.request.method).toBe('POST');
    expect(req.request.body).toEqual({
      internalCode: 'RT001',
      designation: 'Deluxe Room',
      description: 'A deluxe room with all amenities',
      suitableForSurgeries: true
    });

    req.flush({ success: true });
  });
});