# TODO: 가로폭 제한 토글 버튼

## Goal

초대형 모니터에서 텍스트 한 줄이 지나치게 길어져 가독성이 떨어지는 문제를 해결한다.
navbar 우측에 토글 버튼을 추가하여 사용자가 가로폭 제한(max-width) 적용 여부를 직접 선택할 수 있게 한다.

- 기본값: 가로폭 제한 없음 (현재 동작 유지)
- 토글 ON: 콘텐츠 영역에 `max-width: 960px; margin: 0 auto` 적용
- 설정은 `localStorage`에 저장하여 페이지 이동 후에도 유지

## UI 배치

navbar 우측, 계정 메뉴 왼쪽에 아이콘 버튼으로 배치한다.

```
[AhaWiki]  [Navigate▾]  [Page▾]  ···  [⇔]  [👤 닉네임▾]
```

- 아이콘: `fa-arrows-left-right` (폭 제한 OFF) / `fa-compress-alt` (폭 제한 ON)
- 버튼에 `title` 및 `aria-label` 속성으로 설명 제공

## 구현 계획

### 1. LESS 수정 (`app/assets/less.less`)

#### 기본 `.limitWidth` 변경

현재 `width: 97%` 방식은 navbar/header/footer의 padding 방식과 달라 여백이 어색하다.
navbar `.wrap`과 동일하게 padding 방식으로 변경한다.

```less
.AhaWiki .limitWidth {
  padding: 0 24px;       // navbar .wrap과 동일
  box-sizing: border-box;
  // 기존 width: 97%; margin: 0 auto 제거
}

@media (max-width: 767px) {
  .AhaWiki .limitWidth {
    padding: 0 12px;     // 모바일도 기존 margin: 0 12px와 동일 효과
  }
}
```

#### 토글 ON 상태 추가

`body`에 클래스를 붙이는 방식으로 기존 스타일을 건드리지 않고 덮어쓴다.

```less
body.limitedWidth .AhaWiki .limitWidth {
  max-width: 960px;
  margin: 0 auto;
}

### 2. 버튼 HTML 추가 (`app/views/_base.scala.html`)

`actionAuth` ul 바로 앞에 버튼을 삽입한다.

```html
<button type="button" class="widthToggle hotkeyHint"
        aria-label="Toggle content width"
        title="Toggle content width">
    <i class="fas fa-arrows-left-right"></i>
</button>
```

### 3. JavaScript 추가 (`app/views/_base.scala.html` 또는 별도 JS)

```js
(function () {
    const KEY = 'ahawiki-limit-width';
    const $body = $('body');
    const $btn = $('.widthToggle');

    function apply(limited) {
        $body.toggleClass('limitedWidth', limited);
        $btn.find('i')
            .toggleClass('fa-arrows-left-right', !limited)
            .toggleClass('fa-compress-alt', limited);
        $btn.attr('aria-label', limited ? 'Expand content width' : 'Limit content width');
    }

    apply(localStorage.getItem(KEY) === '1');

    $btn.on('click', function () {
        const next = !$body.hasClass('limitedWidth');
        localStorage.setItem(KEY, next ? '1' : '0');
        apply(next);
    });
})();
```

### 4. 모바일 처리

`max-width: 767px` 구간에서는 토글 버튼을 숨긴다 (어차피 창이 좁아서 의미 없음).

```css
@media (max-width: 767px) {
    .widthToggle {
        display: none;
    }
}
```

## Verification Checklist

- [ ] 넓은 모니터(1920px 이상)에서 토글 ON 시 콘텐츠가 960px 이내로 제한되는지 확인
- [ ] 토글 OFF 시 padding 24px 여백으로 복귀하고 navbar와 좌우 정렬이 맞는지 확인
- [ ] 페이지 이동 후에도 설정이 유지되는지 확인 (localStorage)
- [ ] 표/그래프/지도 등 와이드 콘텐츠가 토글 ON 상태에서 잘리지 않는지 확인
- [ ] 모바일(767px 이하)에서 버튼이 보이지 않는지 확인
- [ ] 다크모드 토글 버튼과 나란히 배치되었을 때 시각적으로 자연스러운지 확인
