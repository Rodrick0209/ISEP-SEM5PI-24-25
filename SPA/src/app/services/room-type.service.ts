import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AddRoomType } from '../models/room-type';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class RoomTypeService {
  private url = '/api/roomTypes'

  constructor(private http: HttpClient) { }

  add(roomType: AddRoomType): Observable<any>{
    const body: any = {};
    body.internalCode = roomType.InternalCode;
    body.designation = roomType.Designation;
    if(roomType.Description) body.description = roomType.Description;
    body.suitableForSurgeries = roomType.SuitableForSurgeries;

    return this.http.post(this.url, body);
  }
}
